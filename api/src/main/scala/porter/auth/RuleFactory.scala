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

package porter.auth

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.util.Try

trait RuleFactory {
  import porter.model._

  def permissionFactory: PermissionFactory = customFactory.get() orElse RuleFactory.providedFactory

  final def createRule(rstr: String): Try[Rule] = RuleFactory.createRuleWith(permissionFactory)(rstr)

  private val factories = new AtomicReference[Vector[PermissionFactory]](Vector())
  private val customFactory = new AtomicReference[PermissionFactory](RuleFactory.createFactory(factories.get()))

  @tailrec
  final def register(fac: PermissionFactory) {
    val facs = factories.get()
    val next = fac +: facs
    if (!factories.compareAndSet(facs, next)) {
      register(fac)
    } else {
      customFactory.set(RuleFactory.createFactory(next))
    }
  }

  @tailrec
  final def unregister(fac: PermissionFactory) {
    val facs = factories.get()
    val next = facs filterNot (_ == fac)
    if (!factories.compareAndSet(facs, next)) {
      unregister(fac)
    } else {
      customFactory.set(RuleFactory.createFactory(next))
    }
  }

}

object RuleFactory {
  import porter.model._

  val providedFactory: PermissionFactory = ResourcePermission.factory orElse
    Permission.allPermissionFactory orElse DefaultPermission.factory

  private val emptyFactory: PermissionFactory = PartialFunction.empty
  private def createFactory(factories: Vector[PermissionFactory]) =
    factories.foldRight(emptyFactory){ _ orElse _ }

  def createRuleWith(fac: PermissionFactory)(rstr: String): Try[Rule] = Try {
    if (rstr.charAt(0) != '!') fac(rstr)
    else Revocation(fac(rstr.substring(1)))
  }
}