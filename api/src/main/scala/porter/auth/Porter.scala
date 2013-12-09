package porter.auth

import porter.store.{PropertiesStore, StoreProvider}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 00:56
 */
trait Porter extends AuthC with AuthZ with RuleFactory with StoreProvider with ValidatorProvider