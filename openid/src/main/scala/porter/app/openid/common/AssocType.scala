package porter.app.openid.common

sealed trait AssocType {
  def name: String
  def crypt: Crypt.Hmac
  override def toString = s"AssocType($name)"
}

object AssocType {

  object HmacSha1 extends AssocType {
    val name = "HMAC-SHA1"
    val crypt = Crypt.HmacSha1
  }

  object HmacSha256 extends AssocType {
    val name = "HMAC-SHA256"
    val crypt = Crypt.HmacSha256
  }

  def apply(s: String) = s.toUpperCase match {
    case "HMAC-SHA1" => Some(HmacSha1)
    case "HMAC-SHA256" => Some(HmacSha256)
    case _ => None
  }
}

