package porter.app.akka

package object api {

  case object Unknown extends Serializable

  type PorterMessage = porter.client.Messages.PorterMessage

}
