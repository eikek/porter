package porter.model

import porter.util.ObjectRegistry

object PropertyList extends ObjectRegistry {

  type Elem = Property[_]
  import Property._

  private def reg[T <: Property[_]](prop: T): T = { register(prop); prop }

  /** Property to indicate that an account has been automatically disabled (maybe
    * due to too many failed logins).
    */
  val accountDisabled = reg(BoolProperty("porter-admin-accountDisabled"))

  /** Property to indicate that an account has manually been locked by the admin. */
  val accountLocked = reg(BoolProperty("porter-admin-accountLocked"))

  /** Timestamp of the last successful login */
  val lastLoginTime = reg(TimestampProperty("porter-admin-lastloginTime"))

  /** Number of failed login attempts of some time period. */
  val failedLogins = reg(CounterProperty("porter-admin-failedLogins"))

  /** Number of successful logins of some time period. */
  val successfulLogins = reg(CounterProperty("porter-admin-successfulLogins"))

  val firstName = reg(StringProperty("porter-user-firstName"))
  val lastName = reg(StringProperty("porter-user-lastName"))
  val birthday = reg(StringProperty("porter-user-birthday"))
  val locale = reg(StringProperty("porter-user-locale"))
  val timezone = reg(StringProperty("porter-user-timezone"))
  val firstEmail = reg(StringProperty("porter-user-email-1"))
  val secondEmail = reg(StringProperty("porter-user-email-2"))
  val homepage = reg(StringProperty("porter-user-homepage"))
  val phone = reg(StringProperty("porter-user-phone"))
  val country = reg(StringProperty("porter-user-country"))
  val imtype = reg(StringProperty("porter-user-imtype"))
  val imid = reg(StringProperty("porter-user-imid"))

  def adminProps = all.filter(_.name startsWith "porter-admin")
  def userProps = all.filter(_.name startsWith "porter-user")
}
