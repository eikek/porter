package porter.util

import java.io._
import java.security.SecureRandom
import javax.crypto.spec.{PBEKeySpec, SecretKeySpec, IvParameterSpec}
import javax.crypto._
import scala.io.Codec

/**
 * Utility methods for symmetric encryption using AES.
 *
 * The encryption uses AES in CBC mode. The IV is created randomly and prepended
 * to the cipher text. The first 16 bytes of the encrypted text is the IV.
 *
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 11.02.13 20:49
 */
object AES {

  private val defaultBufferSize = 1024 * 4

  private def copy(in: InputStream, out: OutputStream): Long = {
    var count = 0L
    var num = -1
    val buffer = new Array[Byte](defaultBufferSize)
    do {
      num = in.read(buffer)
      if (num != -1) {
        out.write(buffer, 0, num)
        count = count + num
      }
    } while (num != -1)
    count
  }

  def isLimitedStrength = Cipher.getMaxAllowedKeyLength("AES") == 128

  def deriveKey(password: String, salt: Vector[Byte]): Vector[Byte] = {
    val sfac = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    val strength = if (isLimitedStrength) 128 else 256
    val ks = new PBEKeySpec(password.toCharArray, salt.toArray, math.pow(2, 16).toInt, strength)
    val key = sfac.generateSecret(ks)
    key.getEncoded.toVector
  }

  def generateRandomKey: Vector[Byte] = {
    val kf = KeyGenerator.getInstance("AES")
    val strength = if (isLimitedStrength) 128 else 256
    kf.init(strength)
    kf.generateKey().getEncoded.toVector
  }

  /**
   * Encrypts the given string with the given password. The string's utf8
   * bytes are encrypted and the resulting bytes are returned as an base64
   * encoded string.
   *
   * @param plain
   * @param password
   * @return
   */
  def encryptString(plain: String, password: Vector[Byte]): String = {
    val in = new ByteArrayInputStream(plain.getBytes(Codec.UTF8.charSet))
    val out = new ByteArrayOutputStream()
    encrypt(in, out, password)
    Base64.encode(out.toByteArray)
  }

  /**
   * Expects a base64 encoded string that is first decoded and then decrypted.
   * The decrypted bytes are assumed to be utf8 bytes and are returned as a
   * string.
   *
   * @param ctext
   * @param password
   * @return
   */
  def decryptString(ctext: String, password: Vector[Byte]): String = {
    val in = new ByteArrayInputStream(Base64.decode(ctext).toArray)
    val out = new ByteArrayOutputStream()
    decrypt(in, out, password)
    new String(out.toByteArray, Codec.UTF8.charSet)
  }

  /**
   * Encryptes the bytes from the given input stream and writes them into the given output stream.
   *
   * Note that the outputsteam is closed by this method, since it is required to close the concrete
   * [[javax.crypto.CipherOutputStream]]. Otherwise the final block is not encrypted properly. The
   * input stream, however, is not closed by this method.
   *
   * The IV is created using [[java.security.SecureRandom]] and written as the first 16 bytes into
   * the output stream.
   *
   * @param source
   * @param target
   * @param key
   */
  def encrypt(source: InputStream, target: OutputStream, key: Vector[Byte]) = {
    val ivbytes = new Array[Byte](16)
    SecureRandom.getInstance("SHA1PRNG").nextBytes(ivbytes)
    val iv = new IvParameterSpec(ivbytes)
    target.write(ivbytes)
    val cipher = createCipher(key, iv, Cipher.ENCRYPT_MODE)
    val cout = new CipherOutputStream(target, cipher)
    val result = copy(source, cout)
    cout.close()
    result
  }

  /**
   * Decrypts the bytes from the input stream and writes them into the given outputstream.
   *
   * Note that the input stream is closed by this method, as it is necessary to call it
   * on the concrete [[javax.crypto.CipherInputStream]], otherwise the last block is not
   * decrypted. The given outputstream, however, is not closed by this method.
   *
   * It is assumed that the IV is located at the first 16 bytes in the input stream.
   *
   * @param source
   * @param target
   * @param key
   */
  def decrypt(source: InputStream, target: OutputStream, key: Vector[Byte]) = {
    val ivbytes = new Array[Byte](16)
    source.read(ivbytes)
    val iv = new IvParameterSpec(ivbytes)
    val cipher = createCipher(key, iv, Cipher.DECRYPT_MODE)
    val cin = new CipherInputStream(source, cipher)
    val result = copy(cin, target)
    cin.close()
    result
  }

  def createCipher(key: Vector[Byte], iv: IvParameterSpec, cipherMode: Int) = {
    require(key.length > 0, "A key must be provided.")
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")

    val keyspec = new SecretKeySpec(key.toArray, "AES")
    cipher.init(cipherMode, keyspec, iv)
    cipher
  }
}
