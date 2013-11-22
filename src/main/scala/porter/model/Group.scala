package porter.model

@SerialVersionUID(20131121)
case class Group(name: Ident,
  props: Properties = Map.empty,
  rules: Set[String] = Set.empty) extends Serializable {

  def updateProps(f: Properties => Properties) =  copy(props = f(props))
  def updateRules(f: Set[String] => Set[String]) = copy(rules = f(rules))
}
