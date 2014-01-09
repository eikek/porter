package porter.auth

import porter.model.PasswordCrypt
import scala.util.{Success, Try}

object Nonce {
  import scala.concurrent.duration._

  private val secretString =
    new java.util.concurrent.atomic.AtomicReference[String](java.util.UUID.randomUUID().toString)

  def updateSecret() {
    secretString.set(java.util.UUID.randomUUID().toString)
  }

  /**
   * Generates a string to be used as the server side nonce value. It is comprised
   * of the timestamp and a checksum of a secret string + timestamp.
   *
   * @return
   */
  def generateNonce(timeout: Duration = 2.minutes) = createNonce(System.currentTimeMillis() + timeout.toMillis)

  private def createNonce(timestamp: Long) =
    timestamp + "$" + PasswordCrypt.Bcrypt(10)(timestamp + secretString.get())

  def timestampValid(ts: Long) = System.currentTimeMillis() < ts
  
  def extractTimestamp(nonce: String): Try[Long] = Try {
    val (clientTs, hash) = nonce.indexOf('$') match {
      case idx if idx > 0 => (nonce.substring(0, idx).toLong, nonce.substring(idx+1))
      case _ => sys.error("Invalid nonce")
    }
    if (PasswordCrypt.verify(clientTs + secretString.get(), hash)) clientTs
    else sys.error("Invalid nonce")
  }
  
  def checkNonce(nonce: String): Boolean = extractTimestamp(nonce) match {
    case Success(ts) if timestampValid(ts) => true
    case _ => false
  }
}
