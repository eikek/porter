package porter.model

import scala.io.Codec

@SerialVersionUID(20131121)
case class Secret(name: Ident, data: Vector[Byte]) extends Serializable {
  import scala.io.Codec
  override def toString = s"Secret(${name.name}, ***)"
  lazy val asString = new String(data.toArray, Codec.UTF8.name)
}
object Secret {
  def apply(name: Ident, data: String): Secret = Secret(name, data.getBytes(Codec.UTF8.name).toVector)
  def apply(name: Ident, data: Array[Byte]): Secret = Secret(name, data.toVector)
}