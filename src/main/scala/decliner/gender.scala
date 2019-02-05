package decliner

import io.circe._
import io.circe.parser._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._

import scala.io.Source

sealed abstract class Gender {
  def isMale: Boolean = this == Male
  def isFemale: Boolean = this == Female
  def isAndrogynous: Boolean = this == Androgynous
}
case object Male extends Gender
case object Female extends Gender
case object Androgynous extends Gender

/**
  * Detects the gender by a full name or by one of the name's parts
  */

object Gender {
  def byFullName(lastName: String, firstName: String, middleName: String): Option[Gender] =
    genderDetector.byFullName(lastName, firstName, middleName)
  def byFirstName(firstName: String): Option[Gender] = genderDetector.byFirstName(firstName)
  def byMiddleName(middleName: String): Option[Gender] = genderDetector.byMiddleName(middleName)
  def byLastName(lastName: String): Option[Gender] = genderDetector.byLastName(lastName)

  lazy val genderDetector: GenderDetector = new GenderDetector()

  def all: Seq[Gender] = Seq(Male, Female, Androgynous)
  private def get(name: String): Gender = name match {
    case "male" => Male
    case "female" => Female
    case _ => Androgynous
  }
  implicit val genderDecoder: Decoder[Gender] = (c: HCursor) => c.as[String].map(get)
}

case class GenderRules(androgynous: List[String] = Nil, female: List[String] = Nil, male: List[String] = Nil) {
  def get(gender: Gender): Seq[String] = gender match {
    case Androgynous => androgynous
    case Male => male
    case Female => female
  }
}
case class GenderRuleSet(exceptions: GenderRules = new GenderRules, suffixes: GenderRules)
case class NamePartGenderRules(lastName: GenderRuleSet, firstName: GenderRuleSet, middleName: GenderRuleSet)

class GenderDetector {
  def byLastName(lastName: String): Option[Gender] = byRuleSet(lastName, _.lastName)
  def byFirstName(firstName: String): Option[Gender] = byRuleSet(firstName, _.firstName)
  def byMiddleName(middleName: String): Option[Gender] = byRuleSet(middleName, _.middleName)
  def byFullName(lastName: String, firstName: String, middleName: String): Option[Gender] = {
    val first = byFirstName(firstName)
    val middle = byMiddleName(middleName)
    val last = byLastName(lastName)
    def mergeGenders(res: Option[Gender], gender: Gender): Option[Gender] = (res, gender) match {
      case (None, _) => None
      case (g, Androgynous) => g
      case (Some(Androgynous), g) => Some(g)
      case (r @ Some(g1), g2) if g1 == g2 => r
      case _ => None
    }
    Seq(last, first, middle).map(_.getOrElse(Androgynous)).foldLeft(Some(Androgynous): Option[Gender])(mergeGenders)
  }

  private[this] lazy val genderRules: Option[NamePartGenderRules] = load()

  private implicit val customConfig: Configuration = Configuration.default.withDefaults
  private implicit val genderRulesDecoder: Decoder[GenderRules] = deriveDecoder
  private implicit val genderRuleSetDecoder: Decoder[GenderRuleSet] = deriveDecoder
  private implicit val nameGenderRulesDecoder: Decoder[NamePartGenderRules] = deriveDecoder

  private def load(): Option[NamePartGenderRules] = {
    val parsed = parse(Source.fromResource("genderRules.json").mkString)
    parsed.flatMap(json => json.as[NamePartGenderRules]).toOption
  }

  private def byRuleSet(name: String, ruleSetSelector: NamePartGenderRules => GenderRuleSet): Option[Gender] = {
    val nameLc = name.toLowerCase
    for {
      ruleSet <- genderRules.map(ruleSetSelector)
      inExceptions = byRule(ruleSet.exceptions, r => if (r.startsWith("-")) nameLc.endsWith(r.substring(1)) else r == nameLc)
      matchSuffix = byRule(ruleSet.suffixes, r => nameLc.endsWith(r))
      gender <- inExceptions.orElse(matchSuffix)
    } yield gender
  }

  private def byRule(genderRules: GenderRules, matcher: String => Boolean): Option[Gender] = {
    Gender.all.find(gender => genderRules.get(gender).exists(matcher))
  }
}