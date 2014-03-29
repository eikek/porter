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

import porter.store.{MutableStore, Store}
import com.mongodb.casbah.Imports._
import com.typesafe.config.Config
import scala.util.Try
import porter.model._
import porter.model.Group
import porter.model.Realm
import porter.model.Account
import scala.concurrent.{ExecutionContext, Future}
import porter.util.Base64

class MongoStore(cfg: Config) extends Store with MutableStore {

  private def mongoClient = MongoStore.createClient(cfg)
  private val dbname = Try(cfg.getString("dbname")).getOrElse("porterdb")

  import MongoStore._

  private def withDb[A](body: MongoDB => A): A = {
    val client = mongoClient
    val db = client(dbname)
    try { body(db) } finally { client.close() }
  }

  private def withColl[A](coll: Ident)(body: MongoCollection => A): A = withDb { client =>
    val collection = client(coll.name)
    body(collection)
  }

  def close() {
  }

  def findRealms(names: Set[Ident])(implicit ec: ExecutionContext) = {
    allRealms.map(s => s.filter(r => names.contains(r.id)))
  }

  def findAccounts(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = Future {
    val q = "_id" $in names.map(n => "a:"+ n.name).toSeq
    withColl(realm) { coll =>
      (for (dbo <- coll.find(q)) yield dbo.toAccount).toList
    }
  }

  def findAccountsFor(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext) = {
    val names = creds.collect({ case an: AccountCredentials => an.accountName })
    findAccounts(realm, names)
  }

  def findGroups(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = Future {
    val q = "_id" $in names.map(n => "g:"+n.name).toSeq
    withColl(realm.name) { coll =>
      (for (dbo <- coll.find(q)) yield dbo.toGroup).toList
    }
  }

  def allRealms(implicit ec: ExecutionContext) = Future {
    withDb { client =>
      for {
        coll <- client.collectionNames
        dbo <- client(coll).findOne(MongoDBObject("_id" -> ("r:"+coll)))
      } yield dbo.toRealm
    }
  }

  def allAccounts(realm: Ident)(implicit ec: ExecutionContext) = Future {
    withColl(realm) { coll =>
      (for (dbo <- coll.find(MongoDBObject("type" -> "account")))
       yield dbo.toAccount).toList
    }
  }

  def allGroups(realm: Ident)(implicit ec: ExecutionContext) = Future {
    withColl(realm) { coll =>
      (for {
        dbo <- coll.find(MongoDBObject("type" -> "group"))
      } yield dbo.toGroup).toList
    }
  }

  def updateRealm(realm: Realm)(implicit ec: ExecutionContext) = Future {
    withColl(realm.id) { coll =>
      val result = coll.save(realm.toDBObject)
      result.getN == 1
    }
  }

  def deleteRealm(realm: Ident)(implicit ec: ExecutionContext) = Future {
    withDb { db =>
      val set = db.collectionNames
      if (set.contains(realm.name)) {
        db(realm.name).dropCollection()
        true
      } else {
        throw new IllegalStateException(s"Realm '${realm.name}' does not exist")
      }
    }
  }

  def updateAccount(realm: Ident, account: Account)(implicit ec: ExecutionContext) = Future {
    withColl(realm) { coll =>
      val result = coll.save(account.toDBObject)
      result.getN == 1
    }
  }

  def deleteAccount(realm: Ident, accId: Ident)(implicit ec: ExecutionContext) = Future {
    val q = MongoDBObject("_id" -> s"a:${accId.name}")
    withColl(realm) { coll =>
      val result = coll.remove(q)
      result.getN == 1
    }
  }

  def updateGroup(realm: Ident, group: Group)(implicit ec: ExecutionContext) = Future {
    withColl(realm) { coll =>
      val result = coll.save(group.toDBObject)
      result.getN == 1
    }
  }

  def deleteGroup(realm: Ident, groupId: Ident)(implicit ec: ExecutionContext) = Future {
    val q = MongoDBObject("_id" -> s"g:${groupId.name}")
    withColl(realm) { coll =>
      val result = coll.remove(q)
      result.getN == 1
    }
  }
}

object MongoStore {

  def createClient(cfg: Config): MongoClient = {
    val uri = Try(cfg.getString("uri")).map(MongoClientURI.apply)
    val host = Try(cfg.getString("host")).getOrElse("localhost")
    val port = Try(cfg.getInt("port")).getOrElse(27017)
    uri.map(u => MongoClient(u)).getOrElse(MongoClient(host, port))
  }

  implicit class RealmConv(realm: Realm) {
    def toDBObject = MongoDBObject(
      "_id" -> ("r:"+realm.id.name),
      "name" -> realm.name
    )
  }

  implicit class AccountConv(account: Account) {
    def toDBObject = {
      val groups = MongoDBList(account.groups.map(_.name).toSeq: _*)
      val props = MongoDBObject(account.props.toList)
      val secrets = MongoDBList(account.secrets.map(s => 
        MongoDBObject("name" -> s.name.name, "data" -> Base64.encode(s.data))): _*)
      MongoDBObject(
        "_id" -> s"a:${account.name.name}",
        "props" -> props,
        "groups" -> groups,
        "secrets" -> secrets,
        "type" -> "account"
      )
    }
  }

  implicit class GroupConv(group: Group) {
    def toDBObject = {
      val props = MongoDBObject(group.props.toList)
      val rules = MongoDBList(group.rules.toSeq: _*)
      MongoDBObject(
        "_id" -> s"g:${group.name.name}",
        "props" -> props,
        "rules" -> rules,
        "type" -> "group"
      )
    }
  }

  implicit class DBObjectModel(dbo: DBObject) {
    def toRealm = {
      val id = dbo.get("_id").toString.substring(2)
      val name = dbo.get("name").toString
      Realm(id, name)
    }

    def toAccount = {
      val id = dbo.get("_id").toString.substring(2)
      val props = dbo.getAs[BasicDBObject]("props").get.toList.map(t => (t._1, t._2.toString)).toMap
      val groups = dbo.getAs[MongoDBList]("groups").get.map(x => Ident(x.toString)).toSet
      val secretDbos = dbo.getAs[MongoDBList]("secrets").getOrElse(MongoDBList())
      val secrets = for (so <- secretDbos) yield {
        val smo = so.asInstanceOf[BasicDBObject]
        val name = smo.getAs[String]("name").get
        val data = smo.getAs[String]("data").get
        Secret(name, Base64.decode(data).toVector)
      }
      Account(
        id,
        props,
        groups,
        secrets
      )
    }

    def toGroup = {
      val id = dbo.get("_id").toString.substring(2)
      val props = dbo.getAs[BasicDBObject]("props").get.toList.map(t => (t._1, t._2.toString)).toMap
      val rules = dbo.getAs[MongoDBList]("rules").get.map(_.toString).toSet
      Group(id, props, rules)
    }
  }
}
