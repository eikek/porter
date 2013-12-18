package porter.app.openid

import akka.actor.{ActorRef, Props, Actor}
import java.security.SecureRandom
import scala.concurrent.Future
import scala.Some
import scala.annotation.tailrec
import javax.crypto.SecretKey
import scala.concurrent.duration.FiniteDuration
import porter.app.openid.common.{SessionType, Crypt, AssocType}
import Crypt.DH.DHKeyPair

class AssocActor extends Actor {
  import AssocActor._
  import scala.concurrent.duration._
  import akka.pattern.pipe
  import context.dispatcher

  private val assocTimeout = 5.minutes
  private implicit val random = new SecureRandom()
  private var assocs = Map.empty[String, AssocToken]

  context.system.scheduler.schedule(5.hour, 5.hour, self, Reseed)

  def receive = {
    case Reseed =>
      random.setSeed(SecureRandom.getSeed(16))

    case GetToken(handle) =>
      val token = assocs.get(handle)
      token.filter(_.priv).map { _ =>
        assocs - handle
      }
      sender ! GetTokenResult(handle, token)

    case CreateAssoc(at, st, None) =>
      val macKey = at.crypt.generateKey.get
      val token = AssocToken(at, st, macKey, assocTimeout, priv = false, None)
      self ! UpdateToken(sender, token)

    case CreatePrivateAssoc(at, st) =>
      val macKey = at.crypt.generateKey.get
      val token = AssocToken(at, st, macKey, assocTimeout, priv = true, None)
      self ! UpdateToken(sender, token)

    case CreateAssoc(at, st, Some(DHOptions(p, g, ya))) =>
      val client = sender
      val token = Future {
        val macKey = at.crypt.generateKey.get
        val pubkey = Crypt.DH.publicKey(ya, p, g)
        val keypair = Crypt.DH.generateKeyPair(Crypt.DH.parameterSpec(p, g)).get
        val zz = Crypt.DH.sharedSecret(pubkey, keypair.getPrivate, p)
        val hzz = st.hash(zz.toByteArray.toVector)
        if (hzz.length != macKey.getEncoded.length) sys.error("Invalid lenght of mackey or hzz")
        val macEnc = hzz.zip(macKey.getEncoded.toVector).map {
          case (h, k) => (h ^ k).toByte
        }
        val token = AssocToken(at, st, macKey, assocTimeout, priv = false, Some(DHParams(macEnc, keypair)))
        UpdateToken(client, token)
      }
      token pipeTo self

    case UpdateToken(client, token) =>
      val handle = newHandle
      assocs = assocs.updated(handle, token)
      context.system.scheduler.scheduleOnce(assocTimeout, self, RemoveToken(handle))
      client ! GetTokenResult(handle, Some(token))

    case RemoveToken(handle) =>
      assocs = assocs - handle
  }

  @tailrec
  private def newHandle: String = {
    val h = AssocActor.randomHandle(random)
    if (assocs.contains(h)) newHandle else h
  }

  private case object Reseed
  private case class RemoveToken(handle: String)
  private case class UpdateToken(client: ActorRef, token: AssocToken)
}

object AssocActor {

  def apply(): Props = Props(classOf[AssocActor])

  private def randomHandle(random: java.util.Random) = {
    //33 to 122 is valid according to spec, valid length up to 255
    val range = ('0' to '9') union ('a' to 'z') union ('A' to 'Z')
    val len = (random.nextDouble() * 150).toInt + 18
    def next = (random.nextDouble() * range.length).toInt
    Array.fill(len)(range(next)).mkString("")
  }

  case class DHParams(macEnc: Vector[Byte], keypair: DHKeyPair)
  case class AssocToken(assocType: AssocType, sessionType: SessionType, mac: SecretKey, valid: FiniteDuration, priv: Boolean, dh: Option[DHParams]) {
    private val now = System.currentTimeMillis()
    val expires = now + valid.toMillis
    def isExpired = System.currentTimeMillis() > expires
    def isValid = !isExpired
  }

  case class GetToken(handle: String)
  case class GetTokenResult(handle: String, token: Option[AssocToken])

  case class DHOptions(modulus: BigInt = Crypt.DH.defaultModulus, g: BigInt = 2, public: BigInt)
  case class CreateAssoc(assocType: AssocType, sessionType: SessionType, dh: Option[DHOptions])
  case class CreatePrivateAssoc(at: AssocType, st: SessionType)
}
