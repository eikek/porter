package porter.app.akka.http

import akka.actor.ActorRef
import spray.routing.{Route, Directives}
import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.app.akka.Porter.Messages.mutableStore._
import porter.app.akka.Porter.Messages.store._
import porter.app.akka.PorterUtil
import porter.model._
import porter.auth.Decider
import spray.httpx.SprayJsonSupport
import porter.app.client.PorterAkkaClient

class StoreService(client: PorterAkkaClient)
                  (implicit ec: ExecutionContext, to: Timeout) extends Directives with SprayJsonSupport {

  import PorterJsonProtocol._
  import akka.pattern.ask

  implicit private val timeout = to.duration
  private val porterRef = client.porterRef
  private val accountPrefix = "api" / "account"
  private val groupPrefix = "api" / "group"
  private val realmPrefix = "api" / "realm"
  private val accountId = accountPrefix / Segment / Segment
  private val groupId = groupPrefix / Segment / Segment
  private val realmId = realmPrefix / Segment

  def route: Route = {
    get {
      path(accountId) { (realm, name) =>
        complete {
          PorterUtil.findAccount(porterRef, realm, name)
        }
      } ~
      path(groupId) { (realm, name) =>
        complete {
          PorterUtil.findGroup(porterRef, realm, name)
        }
      } ~
      path(realmId) { realm =>
        complete(PorterUtil.findRealm(porterRef, realm))
      }
    } ~
    delete {
      path(accountId) { (realm, name) =>
        complete {
          client.deleteAccount(DeleteAccount(realm, name))
        }
      } ~
      path(groupId) { (realm, name) =>
        complete {
          client.deleteGroup(DeleteGroup(realm, name))
        }
      } ~
      path(realmId) { realm =>
        complete {
          client.deleteRealm(DeleteRealm(realm))
        }
      }
    } ~
    put {
      path(accountPrefix / Segment) { realm =>
        handleWith { acc: Account =>
          client.updateAccount(UpdateAccount(realm, acc))
        }
      } ~
      path(accountPrefix / "new" / Segment) { realm =>
        handleWith { acc: Account =>
          client.createNewAccount(UpdateAccount(realm, acc))
        }
      } ~
      path(accountPrefix / "updateAuthProps") {
        post {
          handleWith { req: UpdateAuthProps =>
            client.updateAuthProps(req)
          }
        }
      } ~
      path(groupPrefix / Segment) { realm =>
        handleWith { group: Group =>
          client.updateGroup(UpdateGroup(realm, group))
        }
      } ~
      path(realmPrefix) {
        handleWith { realm: Realm =>
          client.updateRealm(UpdateRealm(realm))
        }
      }
    } ~
    post {
      path(accountPrefix / "find") {
        handleWith { req: FindAccounts =>
          client.findAccounts(req)
        }
      } ~
      path(accountPrefix / "update") {
        handleWith { req: UpdateAccount =>
          client.updateAccount(req)
        }
      } ~
      path(accountPrefix / "delete") {
        handleWith { req: DeleteAccount =>
          client.deleteAccount(req)
        }
      } ~
      path(accountPrefix / "new") {
        handleWith { req: UpdateAccount =>
          client.createNewAccount(req)
        }
      } ~
      path(accountPrefix / "changePassword") {
        handleWith { req: ChangePassword =>
          client.changePassword(req)
        }
      } ~
      path(groupPrefix / "find") {
        handleWith { req: FindGroups =>
          client.findGroups(req)
        }
      } ~
      path(groupPrefix / "update") {
        handleWith { req: UpdateGroup =>
          client.updateGroup(req)
        }
      } ~
      path(groupPrefix / "delete") {
        handleWith { req: DeleteGroup =>
          client.deleteGroup(req)
        }
      } ~
      path(realmPrefix / "find") {
        handleWith { req: FindRealms =>
          client.findRealms(req)
        }
      } ~
      path(realmPrefix / "update") {
        handleWith { req: UpdateRealm =>
          client.updateRealm(req)
        }
      } ~
      path(realmPrefix / "delete") {
        handleWith { req: DeleteRealm =>
          client.deleteRealm(req)
        }
      }
    }
  }
}

object StoreService {
  def apply(porterRef: ActorRef, decider: Decider, crypt: PasswordCrypt)
           (implicit ec: ExecutionContext, to: Timeout): StoreService =
    new StoreService(new PorterAkkaClient(porterRef, decider, crypt))
}