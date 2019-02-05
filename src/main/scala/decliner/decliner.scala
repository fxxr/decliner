package decliner

import io.circe._
import io.circe.parser._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._

import scala.io.Source

sealed trait Case
case object Nominative extends Case
case object Genitive extends Case
case object Dative extends Case
case object Accusative extends Case
case object Instrumental extends Case
case object Prepositional extends Case

case class FullName(lastName: String, firstName: String, middleName: String)

/** Decliner functions */

object Decliner {
  def declineLastName(lastName: String, toCase: Case): Option[String] = nameDecliner.declineLastName(lastName, toCase)
  def declineFirstName(firstName: String, toCase: Case): Option[String] = nameDecliner.declineFirstName(firstName, toCase)
  def declineMiddleName(middleName: String, toCase: Case): Option[String] = nameDecliner.declineMiddleName(middleName, toCase)
  def declineFullName(fullName: FullName, toCase: Case): Option[FullName] =
    for {
      lastName <- declineLastName(fullName.lastName, toCase)
      firstName <- declineFirstName(fullName.firstName, toCase)
      middleName <- declineMiddleName(fullName.middleName, toCase)
    } yield FullName(lastName, firstName, middleName)

  lazy val nameDecliner: NameDecliner = new NameDecliner
  lazy val cityDecliner: CityDecliner = new CityDecliner

  def cityIn(cityName: String): Option[String] = cityDecliner.cityIn(cityName)
  def cityFrom(cityName: String): Option[String] = cityDecliner.cityFrom(cityName)
  def cityTo(cityName: String): Option[String] = cityDecliner.cityTo(cityName)
}

case class DeclensionMods(gen: String, dat: String, acc: String, instr: String, prep: String)
case class DeclensionRule(
  gender: Gender = Androgynous,
  test: List[String], 
  mods: DeclensionMods,
  firstWordOnly: Boolean = false
)
case class DeclensionRuleSet(
  exceptions: List[DeclensionRule],
  suffixes: List[DeclensionRule]
)
object DeclensionRuleSet {
  val empty = DeclensionRuleSet(Nil, Nil)
}

class AbstractDecliner {
  type Predicate = String => Boolean

  implicit val customConfig: Configuration = Configuration.default.withDefaults
  implicit val declensionRuleSetDecoder: Decoder[DeclensionRuleSet] = deriveDecoder
  implicit val declensionRuleDecoder: Decoder[DeclensionRule] = deriveDecoder
  implicit val declensionModsDecoder: Decoder[DeclensionMods] = (c: HCursor) => {
    for {
      gen <- c.downN(0).as[String]
      dat <- c.downN(1).as[String]
      acc <- c.downN(2).as[String]
      ins <- c.downN(3).as[String]
      pre <- c.downN(4).as[String]
    } yield DeclensionMods(gen, dat, acc, ins, pre)
  }

  protected def load[T](rulesJsonFile: String)(implicit decoder: Decoder[T], empty: T): T = {
    val parsed = parse(Source.fromResource(rulesJsonFile).mkString)
    parsed.flatMap(json => json.as[T]).toOption.getOrElse(empty)
  }

  def splitWithDelimiters(str: String): List[String] = str.split("((?<=[- ])|(?=[- ]))").toList

  def decline(str: String, toCase: Case, gender: Gender, ruleSet: DeclensionRuleSet): Option[String] = {
    val parts = splitWithDelimiters(str)
    parts match {
      case Nil => None
      case first :: _ =>
        def partDecliner(buf: Seq[Option[String]], part: String) = {
          val rule = findRule(part.toLowerCase, gender, ruleSet, part == first)
          val declined = rule.map(r => applyRule(r, part, toCase))
          buf :+ declined
        }
        sequence(parts.foldLeft(Nil: Seq[Option[String]])(partDecliner)).map(_.mkString)
    }
  }

  def sequence(seq: Seq[Option[String]]): Option[Seq[String]] =
    seq.foldRight(Some(Nil): Option[Seq[String]]){
      (optStr, acc) => (optStr, acc) match {
        case (Some(x), Some(xs)) => Some(x +: xs)
        case _ => None
      }
    }

  def findRule(str: String, gender: Gender, ruleSet: DeclensionRuleSet, firstWord: Boolean): Option[DeclensionRule] = {
    val strLc = str.toLowerCase
    def findException = findExactRule(ruleSet.exceptions, gender, exWord => exWord == strLc, firstWord)
    def findSuffix = findExactRule(ruleSet.suffixes, gender, suffix => strLc.endsWith(suffix), firstWord)
    findException.orElse(findSuffix)
  }
  
  def findExactRule(rules: List[DeclensionRule], gender: Gender, matchFunc: Predicate, firstWord: Boolean): Option[DeclensionRule] = 
    rules.find(rule => firstWordMatches(rule, firstWord) && genderMatches(rule, gender) && wordTest(rule, matchFunc))
  
  private def firstWordMatches(rule: DeclensionRule, firstWord: Boolean) = !rule.firstWordOnly || firstWord
  private def genderMatches(rule: DeclensionRule, gender: Gender) = rule.gender == Androgynous || rule.gender == gender
  private def wordTest(rule: DeclensionRule, matchFunc: Predicate) = rule.test.exists(matchFunc)
  
  def applyRule(rule: DeclensionRule, str: String, toCase: Case): String = {
    val mod = toCase match {
      case Genitive => rule.mods.gen
      case Dative => rule.mods.dat
      case Accusative => rule.mods.acc
      case Instrumental => rule.mods.instr
      case Prepositional => rule.mods.prep
      case _ => ""
    }
    applyMod(str, mod)
  }
  def applyMod(str: String, mod: String): String = mod match {
    case "" => str
    case "." => str
    case m if m.startsWith("-") && str.nonEmpty => applyMod(str.substring(0, str.length - 1), mod.substring(1))
    case _ => str + mod
  }
}