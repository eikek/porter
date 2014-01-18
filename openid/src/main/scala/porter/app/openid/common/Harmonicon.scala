package porter.app.openid.common

import porter.util.Base64
import java.awt.geom.GeneralPath
import java.awt._
import java.awt.image.BufferedImage
import scala.annotation.tailrec
import scala.List

/**
 * Creates a probably unique image to a given string. This is a variant from
 * the [[https://github.com/donpark/identicon identicon]] idea but uses
 * [[https://en.wikipedia.org/wiki/Lissajous_curve Lissajous curves]] by
 * emulating an [[https://en.wikipedia.org/wiki/Harmonograph harmonograph]].
 *
 * The idea was, of course, to create fancy images but this is quite a challenge
 * and unfortunately many images look like some scribbling :).
 *
 * It works by creating a "pendulum equation" for each 4 bytes in the string.
 * The resulting list is divided in two equal lists which are used to calculate
 * the x and y coordinates. Thus, the longer the string, the more those equations
 * are added together. This will lead to more distortion in the image, so its
 * advisable to hash the input string before, for example using md5.
 */
object Harmonicon {

  case class ImageSettings(size: Int = 100, bcolor: Color = Color.WHITE, fcolor: Option[Color] = None, stroke: Stroke = new BasicStroke(0.6f))
  def apply(name: String, time: Timespan = Timespan(), settings: ImageSettings = ImageSettings()): BufferedImage = {
    fromBytes(name.getBytes.toVector, time, settings)
  }
  def fromBytes(bytes: Vector[Byte], time: Timespan = Timespan(), settings: ImageSettings = ImageSettings()): BufferedImage = {
    import java.awt._

    def colorDistance(c1: Color, c2: Color): Double = {
      val dr = c1.getRed - c2.getRed
      val dg = c1.getGreen - c2.getGreen
      val db = c1.getBlue - c2.getBlue
      math.sqrt(math.pow(dr, 2) + math.pow(dg, 2) + math.pow(db, 2))
    }
    def normalizeColor(c: Color): Color =
      if (colorDistance(c, settings.bcolor) < 16d) new Color(c.getRGB ^ 0x00ffffff) else c

    val bi = new BufferedImage(settings.size, settings.size, BufferedImage.TYPE_4BYTE_ABGR)
    val g = bi.getGraphics.asInstanceOf[Graphics2D]
    g.setPaint(settings.bcolor)
    g.fillRect(0, 0, settings.size, settings.size)
    g.translate(settings.size / 2, settings.size / 2)
    g.setStroke(settings.stroke)
    for ((c, points) <- createIconCoors(bytes, settings.size, time)) {
      g.setColor(normalizeColor(settings.fcolor.getOrElse(c)))
      g.draw(makeAwtPath(points))
    }
    g.dispose()
    bi
  }

  def createIconCoors(bytes: Vector[Byte], size: Int = 100, time: Timespan = Timespan()) = {
    val (c, ps) = decodeBytes(bytes)
    val pcs = ps.grouped(ps.length / 2).toList.map(PendulumC.apply)
    val px = pcs(0)
    val py = pcs(1)
    val mx = math.max(2, size / 2 / px.size)
    val my = math.max(2, size / 2 / py.size)
    val points = makeCoors(time, px.withMult(mx), py.withMult(my))
    c zip points.grouped(time.length.toInt / c.length).toList
  }

  def makeAwtPath(points: Seq[(Double, Double)]): GeneralPath = {
    val path = new GeneralPath()
    val h = points.head
    path.moveTo(h._1, h._2)
    points.drop(1).foldLeft(h) { (h, t) =>
      path.lineTo(t._1, t._2)
      t
    }
    path
  }

  def makeCoors(time: Timespan, px: PendulumC, py: PendulumC) =
    time.stream.map { t => (px.getCoor(t), py.getCoor(t)) }

  case class Timespan(max: Double = 20, step: Double = 0.001) {
    require(max > 0, "max argument must be positive")
    require(step > 0, "step argument must be positive")
    def stream: Stream[Double] = {
      def time(s: Double):Stream[Double] = s #:: time(s+step)
      time(- max).takeWhile(_ <= max)
    }
    def length = 1 + (max / step * 2)
  }

  case class Pendulum(A: Double, a: Double, phi: Double, gamma: Double) {
    def getCoor(t: Double) = {
      math.exp(-gamma * t) * A * math.sin(a * t + math.Pi * phi)
    }
  }
  case class PendulumC(ps: List[Pendulum]) {
    def getCoor(t: Double) = ps.foldLeft(0d) { (l, p) => l + p.getCoor(t) }
    def withMult(m: Double) = PendulumC(ps.map(_.copy(A = m)))
    def size = ps.size
  }

  def decodeBytes(bv: Vector[Byte]): (List[Color], List[Pendulum]) = {
    def normBytes(bs: Vector[Byte]): Vector[Byte] = {
      val next = if (bs.length < 5) bs ++ Vector.fill(5 - bs.length)(0.toByte) else bs
      next.length % 4 match {
        case 0 => next
        case x => next ++ Vector.fill(4 - x)(0.toByte)
      }
    }
    val bytes = normBytes(bv)
    val gammas = Seq(-0.01d, 0d, 0.01d, 0.02d)
    val foffset = 2
    val phim = math.Pi * 1.5
    //--- constants

    def makeColor(cb: Seq[Byte]): Color = new Color(cb(0).toInt + 127, cb(1).toInt+127, cb(2).toInt+127)

    def makeNum(b: Seq[Byte]): Double = {
      val bi = BigInt(b.toArray)
      val bd = BigDecimal(bi) / math.pow(10, bi.abs.toString().length)
      bd.toDouble
    }
    def makePendulum(bytes: Seq[Byte]): (Seq[Byte], Pendulum) = {
      val g = gammas(Base64.toDecimal(Base64.toBinary(bytes(0)).take(2)))
      val p = Pendulum(60, foffset + makeNum(bytes.take(2)), phim * makeNum(bytes.drop(2).take(2)), g)
      (bytes.drop(4), p)
    }
    @tailrec
    def makeAll(bytes: Seq[Byte], ps: List[Pendulum]): List[Pendulum] = {
      if (bytes.isEmpty) ps
      else {
        val (rem, p) = makePendulum(bytes)
        makeAll(rem, p :: ps)
      }
    }
    //uses 5 bytes
    val cs = List(makeColor(bytes.take(3)), makeColor(bytes.drop(1).take(3)), makeColor(bytes.drop(2).take(3)))
    (cs, makeAll(bytes, Nil))
  }
}
