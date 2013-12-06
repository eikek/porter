package porter.app.akka

import porter.model.Ident

/**
 *
 * @author <a href="mailto:eike.kettner@gmail.com">Eike Kettner</a>
 * @since 05.12.13 15:16
 */
package object api {

  case object Unknown extends Serializable

  trait PorterMessage extends Serializable {
    def id: Int
  }

  trait RealmMessage extends PorterMessage {
    def realmId: Ident
  }
}
