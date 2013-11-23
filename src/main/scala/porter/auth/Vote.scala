package porter.auth

import porter.model.Ident

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 19:16
 */
@SerialVersionUID(20131122)
sealed trait Vote extends Serializable

object Vote {
  @SerialVersionUID(20131122)
  case object Success extends Vote

  @SerialVersionUID(20131122)
  case class Failed(reasons: Map[Ident, String] = Map.empty) extends Vote

  object Failed {
    def apply(values: (Ident, String)*): Failed = apply(values.toMap)
  }
}