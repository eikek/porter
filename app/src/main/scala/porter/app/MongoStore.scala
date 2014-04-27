/*
 * Copyright 2014 porter <https://github.com/eikek/porter>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package porter.app

import scala.util.Try
import scala.concurrent.{ExecutionContext, Future}
import _root_.akka.actor.ActorSystem
import com.typesafe.config.Config
import reactivemongo.api._
import reactivemongo.bson._
import porter.model._
import porter.model.Group
import porter.model.Realm
import porter.model.Account
import porter.store.{MutableStore, Store}
import porter.util.Base64
import reactivemongo.api.collections.default.BSONCollection

class MongoStore(system: ActorSystem, cfg: Config) extends Store with MutableStore {

  private val dbname = Try(cfg.getString("dbname")).getOrElse("porterdb")
  private val db = MongoStore.createMongo(new MongoDriver(system), cfg).apply(dbname)(system.dispatcher)
  private def collection(name: Ident): BSONCollection = db(name.name)

  import MongoStore._
  final class IdWriter[T](prefix: String)(f: T => Ident) extends BW[T] {
    def write(t: T) = BSONDocument("_id" -> s"$prefix:${f(t).name}")
  }
  implicit val realmIdWriter: BW[RealmId] = new IdWriter[RealmId]("r")(_.name)
  implicit val groupIdWriter: BW[GroupId] = new IdWriter[GroupId]("g")(_.name)
  implicit val accountIdWriter: BW[AccountId] = new IdWriter[AccountId]("a")(_.name)

  def close() {
  }

  def findRealms(names: Set[Ident])(implicit ec: ExecutionContext) = {
    allRealms.map(s => s.filter(r => names.contains(r.id)))
  }

  def findAccounts(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = {
    val q = BSONDocument("_id" -> BSONDocument("$in" -> names.map(n => "a:"+ n.name).toSeq))
    collection(realm).find(q)
      .cursor[Account]
      .collect[List]()
  }

  def findAccountsFor(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext) = {
    val names = creds.collect({ case an: AccountCredentials => an.accountName })
    findAccounts(realm, names)
  }

  def findGroups(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = {
    val q = BSONDocument("_id" -> BSONDocument("$in" -> names.map(n => "g:"+n.name)))
    collection(realm).find(q)
      .cursor[Group]
      .collect[List]()
  }

  def allRealms(implicit ec: ExecutionContext) = {
    def findRealms(names: List[String]) = {
      val f = Future.sequence {
        for (name <- names)
        yield collection(name).find(RealmId(name)).one[Realm]
      }
      f.map(_.flatten)
    }
    for {
      coll <- db.collectionNames
      realms <- findRealms(coll)
    } yield realms
  }

  def allAccounts(realm: Ident)(implicit ec: ExecutionContext) = {
    collection(realm).find(Type.account)
      .cursor[Account]
      .collect[List]()
  }


  def allGroups(realm: Ident)(implicit ec: ExecutionContext) = {
    collection(realm).find(Type.group)
      .cursor[Group]
      .collect[List]()
  }

  def updateRealm(realm: Realm)(implicit ec: ExecutionContext) = {
    collection(realm.id)
      .update(RealmId(realm.id), realm, upsert = true, multi = false)
      .map(e => e.ok)
  }

  def deleteRealm(realm: Ident)(implicit ec: ExecutionContext) = {
    collection(realm).drop()
  }

  def updateAccount(realm: Ident, account: Account)(implicit ec: ExecutionContext) = {
    collection(realm)
      .update(AccountId(account.name), account, upsert = true, multi = false)
      .map(_.ok)
  }

  def deleteAccount(realm: Ident, accId: Ident)(implicit ec: ExecutionContext) = {
    collection(realm).remove(AccountId(accId)).map(_.ok)
  }

  def updateGroup(realm: Ident, group: Group)(implicit ec: ExecutionContext) = {
    collection(realm)
      .update(GroupId(group.name), group, upsert = true, multi = false)
      .map(_.ok)
  }

  def deleteGroup(realm: Ident, groupId: Ident)(implicit ec: ExecutionContext) = {
    collection(realm).remove(GroupId(groupId)).map(_.ok)
  }
}

object MongoStore {

  def createMongo(driver: MongoDriver, cfg: Config): reactivemongo.api.MongoConnection = {
    val uri = Try(cfg.getString("uri")).flatMap(reactivemongo.api.MongoConnection.parseURI)
    val host = Try(cfg.getString("host")).getOrElse("localhost")
    val port = Try(cfg.getInt("port")).getOrElse(27017)
    uri.map(driver.connection).getOrElse(driver.connection(s"$host:$port" :: Nil))
  }

  type BR[T] = BSONDocumentReader[T]
  type BW[T] = BSONDocumentWriter[T]
  type BH[B <: BSONValue, T] = BSONHandler[B, T]

  case class RealmId(name: Ident)
  case class GroupId(name: Ident)
  case class AccountId(name: Ident)

  final class IdHandler[T](prefix: String)(dtor: T => String)(ctor: String => T) extends BH[BSONString, T] {
    def read(bson: BSONString) = {
      val s = bson.value
      if (s startsWith s"$prefix:") ctor(s.substring(2))
      else sys.error(s"Unable to create '$prefix'-id from '$s'")
    }
    def write(t: T) = BSONString(s"$prefix:${dtor(t)}")
  }
  private implicit val realmIdHandler: BH[BSONString, RealmId] = new IdHandler[RealmId]("r")(_.name.name)(RealmId(_))
  private implicit val groupIdHandler: BH[BSONString, GroupId] = new IdHandler[GroupId]("g")(_.name.name)(GroupId(_))
  private implicit val accountIdHandler: BH[BSONString, AccountId] = new IdHandler[AccountId]("a")(_.name.name)(AccountId(_))

  implicit val identHandler: BH[BSONString, Ident] = new BH[BSONString, Ident] {
    def read(bson: BSONString) = bson.value
    def write(t: Ident) = BSONString(t.name)
  }

  implicit val secretHandler: BH[BSONDocument, Secret] = new BH[BSONDocument, Secret] {
    def read(bson: BSONDocument) = {
      val data = bson.getAs[String]("data").map(Base64.decode).getOrElse(Seq.empty)
      Secret(bson.getAs[String]("name").get, data.toArray)
    }
    def write(t: Secret) = {
      BSONDocument(
        "name" -> t.name.name,
        "data" -> Base64.encode(t.data)
      )
    }
  }

  implicit val propertiesHandler: BH[BSONDocument, Properties] = new BH[BSONDocument, Properties] {
    def read(bson: BSONDocument) = {
      val els = bson.elements.collect({ case (k, v: BSONString) => (k, v.value)})
      els.toMap
    }

    def write(t: Properties) = BSONDocument(t.mapValues(BSONString.apply).toSeq)
  }

  case class Type(name: String)
  object Type {
    val account = Type("account")
    val group = Type("group")
  }
  implicit val typeIdWriter: BW[Type] = new BW[Type] {
    def write(t: Type) = BSONDocument("type" -> t.name)
  }

  implicit val realmReader: BR[Realm] = new BR[Realm] {
    def read(bson: BSONDocument) = {
      Realm(
        bson.getAs[RealmId]("_id").get.name,
        bson.getAs[String]("name").getOrElse("")
      )
    }
  }
  implicit val realmWriter: BW[Realm] = new BW[Realm] {
    def write(t: Realm) = BSONDocument (
      "_id" -> RealmId(t.id),
      "name" -> t.name,
      "type" -> "realm"
    )
  }

  implicit val accountReader: BR[Account] = new BR[Account] {
    def read(bson: BSONDocument) = {
      val groups = bson.getAs[Set[Ident]]("groups").getOrElse(Set.empty)
      val props = bson.getAs[Properties]("props").getOrElse(Map.empty)
      val secrets = bson.getAs[Seq[Secret]]("secrets").getOrElse(Seq.empty)
      Account(
        bson.getAs[AccountId]("_id").get.name,
        props,
        groups,
        secrets
      )
    }
  }

  implicit val accountWriter: BW[Account] = new BW[Account] {
    def write(t: Account) = BSONDocument(
      "_id" -> AccountId(t.name),
      "props" -> t.props,
      "groups" -> t.groups,
      "secrets" -> t.secrets,
      "type" -> "account"
    )
  }

  implicit val groupReader: BR[Group] = new BR[Group] {
    def read(bson: BSONDocument) = {
      val props = bson.getAs[Properties]("props").getOrElse(Map.empty)
      val rules = bson.getAs[Set[String]]("rules").getOrElse(Set.empty)
      Group(
        bson.getAs[GroupId]("_id").get.name,
        props,
        rules
      )
    }
  }

  implicit val groupWriter: BW[Group] = new BW[Group] {
    def write(t: Group) = BSONDocument(
      "_id" -> GroupId(t.name),
      "props" -> t.props,
      "rules" -> t.rules,
      "type" -> "group"
    )
  }
}
