package decliner

import io.circe._
import io.circe.generic.extras.semiauto._

class NameDecliner extends AbstractDecliner {
  def declineLastName(lastName: String, toCase: Case): Option[String] =
    declineName(lastName, toCase, Gender.byLastName, nameRules.lastName)
  def declineFirstName(firstName: String, toCase: Case): Option[String] =
    declineName(firstName, toCase, Gender.byFirstName, nameRules.firstName)
  def declineMiddleName(middleName: String, toCase: Case): Option[String] =
    declineName(middleName, toCase, Gender.byMiddleName, nameRules.middleName)

  private def declineName(name: String, toCase: Case, genderDetector: String => Option[Gender], ruleSet: DeclensionRuleSet): Option[String] =
    decline(name, toCase, genderDetector(name).getOrElse(Androgynous), ruleSet)

  case class NameRules(firstName: DeclensionRuleSet, middleName: DeclensionRuleSet, lastName: DeclensionRuleSet)
  private[decliner] lazy val nameRules: NameRules = load[NameRules]("nameRules.json")
  
  implicit def nameRulesDecoder: Decoder[NameRules] = deriveDecoder
  implicit def emptyNameRules: NameRules = NameRules(DeclensionRuleSet.empty, DeclensionRuleSet.empty, DeclensionRuleSet.empty)  
}