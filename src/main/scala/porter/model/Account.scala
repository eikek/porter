package porter.model

@SerialVersionUID(20131121)
case class Account(name: Ident,
  props: Properties = Map.empty,
  groups: Set[Ident] = Set.empty,
  secrets: Seq[Secret] = Seq.empty) extends Serializable {

  def updateProps(f: Properties => Properties) =  copy(props = f(props))
  def updateGroups(f: Set[Ident] => Set[Ident]) = copy(groups = f(groups))
  def updateSecrets(f: Seq[Secret] => Seq[Secret]) = copy(secrets = f(secrets))

  /**
   * Replaces (or just adds) a secret with same name with the given one.
   *
   * @param s
   * @return
   */
  def changeSecret(s: Secret) = updateSecrets { secs =>
    secs.filterNot(_.name == s.name) :+ s
  }

  def memberOf(group: Ident) = groups contains group
}
