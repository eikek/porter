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

package porter.app.openid.common

import scala.util.Try
import java.security._
import javax.crypto.{Mac, KeyGenerator, SecretKey}
import java.security.spec.AlgorithmParameterSpec
import javax.crypto.spec.{DHPublicKeySpec, SecretKeySpec, DHGenParameterSpec, DHParameterSpec}
import javax.crypto.interfaces.{DHPrivateKey, DHPublicKey}
import porter.util.Base64

object Crypt {

  object DH {
    import scala.language.implicitConversions
    private implicit def toJavaBigInteger(bi: BigInt) = bi.bigInteger
    private implicit def toScalaBigInt(bi: java.math.BigInteger) = BigInt(bi)

    val name = "DH"

    //1024bit prime as specified in http://openid.net/specs/openid-authentication-2_0.html
    val defaultModulus = BigInt("DCF93A0B883972EC0E19989AC5A2CE310" +
      "E1D37717E8D9571BB7623731866E61EF75A2E27898B057F9891C2E27A639C3F29B6" +
      "0814581CD3B2CA3986D2683705577D45C2E7E52DC81C7A171876E5CEA74B1448BFD" +
      "FAF18828EFD2519F14E45E3826634AF1949E5B535CC829A483B8A76223E5D490A25" +
      "7F05BDFF16F2FB22C583AB", 16)

    val defaultModulusBase64 = Base64.encode(defaultModulus.toByteArray)

    val defaultG = BigInt("2")
    val defaultGBase64 = Base64.encode(defaultG.toByteArray)

    def generateKeyPair(spec: AlgorithmParameterSpec) =
      Crypt.generateKeyPair(name)(spec).map(new DHKeyPair(_))

    def generateRandomKeyPair(primeSize: Int = 1024, keystrength: Int = 256) =
      generateRandomParams(primeSize, keystrength).flatMap(generateKeyPair)

    def defaultParameter = parameterSpec(defaultModulus, BigInt(2))

    def parameterSpec(p: BigInt, g: BigInt) = new DHParameterSpec(p, g)

    def publicKey(yb: BigInt, p: BigInt, g: BigInt) = {
      val spec = new DHPublicKeySpec(yb, p, g)
      val kf = KeyFactory.getInstance(name)
      kf.generatePublic(spec).asInstanceOf[DHPublicKey]
    }

    def generateRandomParams(primeSize: Int = 1024, keystrength: Int = 256): Try[DHParameterSpec] = Try {
      val paramGen = AlgorithmParameterGenerator.getInstance(name)
      val spec = new DHGenParameterSpec(primeSize, keystrength)
      paramGen.init(spec)
      val params = paramGen.generateParameters()
      params.getParameterSpec(classOf[DHParameterSpec])
    }

    def sharedSecret(yb: BigInt, xa: BigInt, p: BigInt = defaultModulus): BigInt =
      yb.modPow(xa, p)

    def sharedSecret(yb: DHPublicKey, xa: DHPrivateKey, p: BigInt): BigInt =
      sharedSecret(yb.getY, xa.getX, p)

    final class DHKeyPair(kp: KeyPair) extends Serializable {
      def getPublic: DHPublicKey = kp.getPublic.asInstanceOf[DHPublicKey]
      def getPrivate: DHPrivateKey = kp.getPrivate.asInstanceOf[DHPrivateKey]
    }
  }

  final class Hmac private[Crypt](val name: String, strength: Int) {
    def generateKey = Crypt.generateKey(name)(strength)
    def createKey(data: Vector[Byte]) = new SecretKeySpec(data.toArray, name)
    def sign(key: SecretKey, data: Vector[Byte]) = Crypt.sign(key, data)
  }

  val HmacSha1: Hmac = new Hmac("HmacSHA1", 160)
  val HmacSha256: Hmac = new Hmac("HmacSHA256", 256)

  def generateKeyPair(algorithm: String)(spec: AlgorithmParameterSpec): Try[KeyPair] = Try {
    val gen = KeyPairGenerator.getInstance(algorithm)
    gen.initialize(spec)
    gen.generateKeyPair()
  }

  def generateKey(algorithm: String)(strength: Int): Try[SecretKey] = Try {
    val gen = KeyGenerator.getInstance(algorithm)
    gen.init(strength)
    gen.generateKey()
  }

  def sign(key: SecretKey, data: Vector[Byte]): Try[Vector[Byte]] = Try {
    val algo = key.getAlgorithm
    val mac = Mac.getInstance(algo)
    mac.init(key)
    mac.doFinal(data.toArray).toVector
  }

  def verifySig(key: SecretKey)(signature: Vector[Byte], data: Vector[Byte]): Try[Boolean] = Try {
    val sigx = sign(key, data).get
    if (signature.length != sigx.length) false
    else signature.zip(sigx).foldLeft(true) { case (b, (s1, s2)) =>
      b && (s1 == s2)
    }
  }
}
