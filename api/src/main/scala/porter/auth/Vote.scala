package porter.auth

import porter.model.Ident

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 19:16
 */
@SerialVersionUID(20131122)
sealed trait Vote extends Serializable {
  def toBoolean: Boolean
  def isFailed: Boolean
  def isSuccess: Boolean
}

object Vote {
  @SerialVersionUID(20131122)
  case object Success extends Vote {
    val toBoolean = true
    val isFailed = false
    val isSuccess = true
  }

  @SerialVersionUID(20131122)
  final case class Failed(reasons: Map[Ident, String] = Map.empty) extends Vote {
    val toBoolean = false
    val isFailed = true
    val isSuccess = false
  }

  object Failed {
    def apply(values: (Ident, String)*): Failed = apply(values.toMap)
  }
}