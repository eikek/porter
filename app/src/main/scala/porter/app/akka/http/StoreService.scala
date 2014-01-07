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

class StoreService(porterRef: ActorRef, decider: Decider, crypt: PasswordCrypt)
                  (implicit ec: ExecutionContext, to: Timeout) extends Directives with SprayJsonSupport {

  import PorterJsonProtocol._
  import akka.pattern.ask

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
          (porterRef ? DeleteAccount(realm, name)).mapTo[OperationFinished]
        }
      } ~
      path(groupId) { (realm, name) =>
        complete {
          (porterRef ? DeleteGroup(realm, name)).mapTo[OperationFinished]
        }
      } ~
      path(realmId) { realm =>
        complete {
          (porterRef ? DeleteRealm(realm)).mapTo[OperationFinished]
        }
      }
    } ~
    put {
      path(accountPrefix / Segment) { realm =>
        handleWith { acc: Account =>
          (porterRef ? UpdateAccount(realm, acc)).mapTo[OperationFinished]
        }
      } ~
      path(accountPrefix / "new" / Segment) { realm =>
        handleWith { acc: Account =>
          PorterUtil.createNewAccount(porterRef, realm, acc)
            .map(_ => OperationFinished(result = true))
            .recover({ case x => OperationFinished(result = false) })
        }
      } ~
      path(groupPrefix / Segment) { realm =>
        handleWith { group: Group =>
          (porterRef ? UpdateGroup(realm, group)).mapTo[OperationFinished]
        }
      } ~
      path(realmPrefix) {
        handleWith { realm: Realm =>
          (porterRef ? UpdateRealm(realm)).mapTo[OperationFinished]
        }
      }
    } ~
    post {
      path(accountPrefix / "find") {
        handleWith { req: FindAccounts =>
          (porterRef ? req).mapTo[FindAccountsResp]
        }
      } ~
      path(accountPrefix / "update") {
        handleWith { req: UpdateAccount =>
          (porterRef ? req).mapTo[OperationFinished]
        }
      } ~
      path(accountPrefix / "delete") {
        handleWith { req: DeleteAccount =>
          (porterRef ? req).mapTo[OperationFinished]
        }
      } ~
      path(accountPrefix / "new") {
        handleWith { req: UpdateAccount =>
          PorterUtil.createNewAccount(porterRef, req.realmId, req.account)
            .map(_ => OperationFinished(result = true))
            .recover({ case x => OperationFinished(result = false) })
        }
      } ~
      path(accountPrefix / "changePassword") {
        handleWith { req: ChangePassword =>
          PorterUtil.changePassword(porterRef, req.realm, req.current, req.plain, crypt, decider)
            .map(_ => OperationFinished(result = true))
            .recover({ case x => OperationFinished(result = false)})
        }
      } ~
      path(groupPrefix / "find") {
        handleWith { req: FindGroups =>
          (porterRef ? req).mapTo[FindGroupsResp]
        }
      } ~
      path(groupPrefix / "update") {
        handleWith { req: UpdateGroup =>
          (porterRef ? req).mapTo[OperationFinished]
        }
      } ~
      path(groupPrefix / "delete") {
        handleWith { req: DeleteGroup =>
          (porterRef ? req).mapTo[OperationFinished]
        }
      } ~
      path(realmPrefix / "find") {
        handleWith { req: FindRealms =>
          (porterRef ? req).mapTo[FindRealmsResp]
        }
      } ~
      path(realmPrefix / "update") {
        handleWith { req: UpdateRealm =>
          (porterRef ? req).mapTo[OperationFinished]
        }
      } ~
      path(realmPrefix / "delete") {
        handleWith { req: DeleteRealm =>
          (porterRef ? req).mapTo[OperationFinished]
        }
      }
    }
  }
}

object StoreService {
  def apply(porterRef: ActorRef, decider: Decider, crypt: PasswordCrypt)
           (implicit ec: ExecutionContext, to: Timeout): StoreService = new StoreService(porterRef, decider, crypt)
}