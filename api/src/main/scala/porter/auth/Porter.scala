package porter.auth

import porter.store.StoreProvider

trait Porter extends AuthC with AuthZ with RuleFactory with StoreProvider with ValidatorProvider