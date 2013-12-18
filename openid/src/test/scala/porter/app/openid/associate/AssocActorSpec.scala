package porter.app.openid.associate

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, WordSpec}
import porter.app.openid.{AssocActor}
import AssocActor.{DHParams, DHOptions, AssocToken}
import porter.util.Base64
import javax.crypto.spec.DHPublicKeySpec
import java.security.KeyFactory
import javax.crypto.interfaces.DHPublicKey
import porter.app.openid.common.{SessionType, Crypt, AssocType}

class AssocActorSpec extends TestKit(ActorSystem("AssocActorSpec", ConfigFactory.load("reference")))
  with WordSpec with BeforeAndAfterAll with ImplicitSender {
  import scala.language.implicitConversions
  private implicit def toJavaBigInteger(bi: BigInt) = bi.bigInteger
  private implicit def toScalaBigInt(bi: java.math.BigInteger) = BigInt(bi)

  val p = Crypt.DH.defaultModulus
  val g = Crypt.DH.defaultParameter.getG

  val ckpair = Crypt.DH.generateKeyPair(Crypt.DH.defaultParameter).get

  "A AssocActor" must {

    "create valid dh tokens" in {
      val actor = system.actorOf(AssocActor())
      actor ! AssocActor.CreateAssoc(AssocType.HmacSha256, SessionType.DHSha256, Some(DHOptions(public = ckpair.getPublic.getY)))
      val AssocActor.GetTokenResult(h, Some(AssocToken(at, st, mac, valid, priv, Some(DHParams(macEnc, skpair))))) =
        expectMsgAnyClassOf(classOf[AssocActor.GetTokenResult])

      assert(at === AssocType.HmacSha256)
      assert(st === SessionType.DHSha256)
      val zz = Crypt.DH.sharedSecret(skpair.getPublic, ckpair.getPrivate, p)
      assert(Crypt.DH.sharedSecret(ckpair.getPublic, skpair.getPrivate, p) === zz)
      val dec = macEnc.zip(st.hash(zz.toByteArray.toVector)).map {
        case (s0, s1) => (s0 ^ s1).toByte
      }
      assert(dec === mac.getEncoded.toVector)
      assert(valid.toSeconds > 0)
    }

    "encrypt the mac key" in {
      val actor = system.actorOf(AssocActor())
      actor ! AssocActor.CreateAssoc(AssocType.HmacSha256, SessionType.DHSha256, Some(DHOptions(p, g, ckpair.getPublic.getY)))
      val AssocActor.GetTokenResult(h, Some(AssocToken(at, st, mac, valid, priv, Some(DHParams(macEnc, skpair))))) =
        expectMsgAnyClassOf(classOf[AssocActor.GetTokenResult])

      val hzz = getDigestZZ(Base64.encode(skpair.getPublic.getY.toByteArray), SessionType.DHSha256)
      assert (hzz.length === macEnc.length, "length are not equal")

      val decoded = hzz.zip(macEnc).map({ case (a, b) => (a ^ b).toByte })
      assert(decoded === mac.getEncoded.toVector)
    }

    "after create returns, a get must be successful" in {
      val actor = system.actorOf(AssocActor())
      actor ! AssocActor.CreateAssoc(AssocType.HmacSha256, SessionType.NoEncryption, None)
      val c1 = expectMsgClass(classOf[AssocActor.GetTokenResult])

      actor ! AssocActor.GetToken(c1.handle)
      expectMsg(c1)
    }
  }

  def getDigestZZ(pubkeyB64: String, stype: SessionType) = {
    val dec = BigInt(Base64.decode(pubkeyB64).toArray)
    val pubkey = Crypt.DH.publicKey(dec, p, g)

    val privkey = ckpair.getPrivate
    val zz = Crypt.DH.sharedSecret(pubkey, privkey, p)
    stype.hash(zz.toByteArray.toVector)
  }
}
