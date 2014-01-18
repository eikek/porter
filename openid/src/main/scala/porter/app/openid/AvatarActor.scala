/*
 * Copyright 2014 porter <https://github.com/eikek/porter>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package porter.app.openid

import akka.actor._
import porter.model.{PropertyList, Ident}
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO
import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import porter.app.openid.common.Harmonicon
import java.awt.image.BufferedImage
import java.awt.RenderingHints
import akka.util.Timeout
import porter.util.Hash
import porter.client.Messages.PorterMessage
import porter.app.openid.CacheDirActor._
import porter.app.openid.AvatarActor.AvatarImageResp
import porter.app.openid.CacheDirActor.FileResponse
import porter.app.openid.common.Harmonicon.Timespan
import scala.Some
import porter.model.Property.BinaryValue
import porter.app.openid.CacheDirActor.FindFile
import porter.app.openid.common.Harmonicon.ImageSettings
import porter.app.openid.AvatarActor.GetAvatarImage
import porter.app.openid.CacheDirActor.WriteImage
import porter.client.Messages.store.FindAccountsResp
import porter.app.openid.CacheDirActor.CacheDirOpts
import porter.client.Messages.store.FindAccounts

/**
 * Generates or retrieves avatar images for an account.
 *
 * If the `avatar` property of an account is set, the bytes are read into an image and
 * scaled according to the request. If `cacheDirOpts` is specified, the bytes are saved
 * as PNG image to the local file system. To avoid unlimited growth, a max size (in MB)
 * can be specified. The actor will delete *all* PNG files in that directory until enough
 * space is freed to store the next avatar image.
 *
 * If the `avatar` property is not set for an account, or the account is not found, an
 * [[porter.app.openid.common.Harmonicon]] image is generated. The `cacheDirOpts` are
 * used, too, if specified.
 *
 * TODO: choose better what files to delete, right now it will delete files randomly as
 * returned from underlying filesystem
 *
 * @param porterRef
 * @param cacheDirOpts
 */
class AvatarActor(porterRef: ActorRef, cacheDirOpts: Option[CacheDirOpts]) extends Actor with ActorLogging {
  implicit val timeout = Timeout(8000)
  import AvatarActor._

  val cachedir = context.actorOf(CacheDirActor(cacheDirOpts), name = "image-cache-dir")

  def receive: Actor.Receive = {
    case m: GetAvatarImage =>
      val worker = context.actorOf(AvatarWorker(sender, porterRef, cachedir))
      worker forward m
    case ClearCacheDir =>
      cachedir ! ClearCacheDir
  }

  override def supervisorStrategy = new OneForOneStrategy(10)(SupervisorStrategy.defaultDecider) {
    override def processFailure(context: ActorContext, restart: Boolean, child: ActorRef, cause: Throwable, stats: ChildRestartStats, children: Iterable[ChildRestartStats]) = {
      if (child == cachedir)
        super.processFailure(context, restart, child, cause, stats, children)
      else
        context.stop(child)
    }
  }
}

object AvatarActor {
  def apply(porterRef: ActorRef, cacheDir: Option[CacheDirOpts]) = Props(classOf[AvatarActor], porterRef, cacheDir)

  case class GetAvatarImage(realm: Ident, account: Ident, size: Int) extends PorterMessage
  case class AvatarImageResp(account: Ident, size: Int, contentType: String, data: Array[Byte], lastModified: Option[Long]) extends PorterMessage
}

private class CacheDirActor(opts: Option[CacheDirOpts]) extends Actor with ActorLogging {
  import CacheDirActor._
  private val emptyResponse = FileResponse(None)
  var cachedirSize = 0L

  def receive = opts.map(active) getOrElse inactive

  def active(opts: CacheDirOpts): Receive = {
    case FindFile(name) =>
      val f = Option(opts.dir.resolve(name)).filter(f => Files.isReadable(f) && Files.isRegularFile(f))
      sender ! FileResponse(f)

    case WriteImage(bytes, name) =>
      val target = opts.dir.resolve(name)
      if (!Files.isReadable(target)) {
        log.debug(s"Writing image file $name ...")
        val removed = makeSpaceFor(bytes.length, cachedirSize, opts)
        Files.write(target, bytes)
        cachedirSize = cachedirSize + bytes.length - removed
        log.debug(s"New cache dir size is $cachedirSize")
      }

    case ClearCacheDir =>
      log.info("Clearing the image cache directory")
      opts.fileIter foreach Files.delete
  }

  def inactive: Receive = {
    case FindFile(_) =>
      sender ! emptyResponse
  }

  override def preStart() = {
    opts.map(_.dir).map { dir =>
      if (!Files.exists(dir)) {
        log.info(s"Create cache directory at '${dir.toAbsolutePath}'")
        Files.createDirectories(dir)
      }
      require(Files.exists(dir) && Files.isDirectory(dir) && Files.isWritable(dir), "Cachedir not writable or not a directory")
      cachedirSize = opts.get.fileIter.map(Files.size).foldLeft(0L){ (s, e) => s+e }
    }
  }
}
object CacheDirActor {

  def apply(opts: Option[CacheDirOpts]) = Props(classOf[CacheDirActor], opts)

  case class FindFile(name: String)
  case class FileResponse(file: Option[Path]) {
    def toBytes = file.map(Files.readAllBytes)
    def lastModification = file.map(Files.getLastModifiedTime(_)).map(_.toMillis)
  }
  case class WriteImage(bytes: Array[Byte], name: String)
  case object ClearCacheDir

  case class CacheDirOpts(dir: Path, maxSizeMb: Int = 20) {
    val maxBytes = maxSizeMb * 1024L * 1024L
    def fileIter = {
      import collection.JavaConverters._
      Files.newDirectoryStream(dir, "*.png").asScala
    }
  }

  private def makeSpaceFor(len: Int, curSize: Long, opts: CacheDirOpts): Long = {
    if (len + curSize < opts.maxBytes) 0
    else {
      val freespace = curSize - opts.maxBytes - len
      @scala.annotation.tailrec
      def removeFiles(fiter: Iterator[Path] = opts.fileIter.iterator, sum: Long = 0): Long = {
        if (sum >= freespace || !fiter.hasNext) sum
        else {
          val p = fiter.next()
          val sz = Files.size(p)
          Files.delete(p)
          removeFiles(fiter, sum + sz)
        }
      }
      removeFiles()
    }
  }
}

private class AvatarWorker(client: ActorRef, porterRef: ActorRef, cacheDir: ActorRef) extends Actor with ActorLogging {
  import AvatarWorker._

  def receive: Receive = starting orElse stopOnUnexpected

  def starting: Receive = {
    case req@GetAvatarImage(realm, account, size) =>
      porterRef ! FindAccounts(realm, Set(account))
      context.become(waitForAccount(req) orElse stopOnUnexpected)
  }

  def waitForAccount(req: GetAvatarImage): Receive = {
    case FindAccountsResp(accounts) =>
      val size = if (req.size < 16) 16 else if (req.size > 700) 700 else req.size
      val avatar = accounts.headOption.flatMap(a => PropertyList.avatar.get(a.props).map(p => a -> p))
      avatar match {
        case Some((acc, binval)) if binval.contentType.startsWith("image/") =>
          val fname = Hash.toHexBinary(Hash.md5(binval.data.toVector)) +"_"+ size +".png"
          cacheDir ! FindFile(fname)
          context.become(waitForAccountFile(req, binval, size, fname) orElse stopOnUnexpected)
        case _ =>
          val fname = Hash.md5String(req.account.name) +"_"+ size +".png"
          cacheDir ! FindFile(fname)
          context.become(waitForHarmoniconFile(req, size, fname) orElse stopOnUnexpected)
      }
  }

  def waitForAccountFile(req: GetAvatarImage, binval: BinaryValue, size: Int, fname: String): Receive = {
    case r: FileResponse =>
      val bytes = r.toBytes getOrElse createScaledImage(binval.data, size)
      client ! AvatarImageResp(req.account.name, size, "image/png", bytes, r.lastModification)
      cacheDir ! WriteImage(bytes, fname)
      context.stop(self)
  }

  def waitForHarmoniconFile(req: GetAvatarImage, size: Int, fname: String): Receive = {
    case r: FileResponse =>
      val bytes = r.toBytes getOrElse createHarmonicon(req.account, size)
      client ! AvatarImageResp(req.account.name, size, "image/png", bytes, r.lastModification)
      cacheDir ! WriteImage(bytes, fname)
      context.stop(self)
  }

  def stopOnUnexpected: Receive = {
    case x =>
      log.error("Unexpected message: "+ x)
      context.stop(self)
  }
}

object AvatarWorker {

  def apply(client: ActorRef, porterRef: ActorRef, cacheDir: ActorRef) = Props(classOf[AvatarWorker], client, porterRef, cacheDir)

  private def imageToBytes(img: BufferedImage): Array[Byte] = {
    val bytes = new ByteArrayOutputStream()
    ImageIO.write(img, "PNG", bytes)
    bytes.toByteArray
  }

  private def createScaledImage(data: Array[Byte], size: Int): Array[Byte] = {
    val bytes = new ByteArrayInputStream(data)
    imageToBytes(scaleImage(size, ImageIO.read(bytes)))
  }

  private def createHarmonicon(name: Ident, size: Int): Array[Byte] = {
    imageToBytes(Harmonicon(Hash.md5String(name.name), Timespan(), ImageSettings(size)))
  }

  private def scaleImage(size: Int, img: BufferedImage): BufferedImage = {
    if (img.getWidth == size || img.getHeight == size) img
    else {
      val tmp = new BufferedImage(size, size, BufferedImage.TYPE_INT_ARGB)
      val g2 = tmp.createGraphics()
      g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g2.drawImage(img, 0, 0, size, size, null)
      g2.dispose()
      tmp
    }
  }
}