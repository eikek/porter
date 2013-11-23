package porter.auth

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.util.Try

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 00:08
 */
trait RuleProvider {
  import porter.model._

  def permissionFactory: PermissionFactory = RuleProvider.defaultFactory

  def createRule(rstr: String): Try[Rule] = createRuleWith(permissionFactory)(rstr)

  def createRuleWith(fac: PermissionFactory)(rstr: String): Try[Rule] = Try {
    if (rstr.charAt(0) != '!') fac(rstr)
    else Revocation(fac(rstr.substring(1)))
  }
}

object RuleProvider {
  import porter.model._

  private val emptyFactory: PermissionFactory = PartialFunction.empty
  private val factories = new AtomicReference[Vector[PermissionFactory]](Vector())

  private[this] def createCustomFactory = factories.get().foldRight(emptyFactory){ _ orElse _ }

  private val customFactory = new AtomicReference[PermissionFactory](createCustomFactory)
  private val providedFactory: PermissionFactory = ResourcePermission.factory orElse DefaultPermission.factory

  def defaultFactory: PermissionFactory = customFactory.get() orElse providedFactory

  @tailrec
  def register(fac: PermissionFactory) {
    val facs = factories.get()
    val next = fac +: facs
    if (!factories.compareAndSet(facs, next)) {
      register(fac)
    } else {
      customFactory.set(createCustomFactory)
    }
  }

  @tailrec
  def unregister(fac: PermissionFactory) {
    val facs = factories.get()
    val next = facs filterNot (_ == fac)
    if (!factories.compareAndSet(facs, next)) {
      unregister(fac)
    } else {
      customFactory.set(createCustomFactory)
    }
  }
}