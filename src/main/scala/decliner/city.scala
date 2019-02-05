package decliner

import io.circe._



class CityDecliner extends AbstractDecliner {
  def cityIn(name: String): Option[String] = declineCity(name, Prepositional)
  def cityFrom(name: String): Option[String] = declineCity(name, Genitive)
  def cityTo(name: String): Option[String] = {
    val parts = splitWithDelimiters(name)
    parts match {
      case Nil => None
      case _ =>
        def declinePart(part: String): Option[String] = part.toLowerCase match {
          case p if p.endsWith("а") => Some(applyMod(part, "-у"))
          case p if p.endsWith("ая") => Some(applyMod(part, "--ую"))
          case p if p.endsWith("ия") => Some(applyMod(part, "-ю"))
          case _ => Some(part)
        }
        sequence(parts.map(p => if (isFrozenPart(p, parts)) Some(p) else declinePart(p))).map(_.mkString)
    }
  }

  case class CityRules(frozenParts: List[String], frozenPartsAfter: List[String], ruleSet: DeclensionRuleSet)

  private[decliner] lazy val cityRules: CityRules = load[CityRules]("cityRules.json")

  implicit def emptyRules: CityRules = CityRules(Nil, Nil, DeclensionRuleSet.empty)
  implicit def cityRulesDecoder: Decoder[CityRules] = (c: HCursor) => for {
    frozenParts <- c.downField("frozenParts").as[List[String]]
    frozenPartsAfter <- c.downField("frozenPartsAfter").as[List[String]]
    ruleSet <- c.downField("city").as[DeclensionRuleSet]
  } yield CityRules(frozenParts, frozenPartsAfter, ruleSet)
  
  private def declineCity(name: String, toCase: Case): Option[String] = {
    val parts = splitWithDelimiters(name)
    parts match {
      case Nil => None
      case first :: _ =>
        def declinePart(part: String): Option[String] = {
          val cityRule = findRule(part, Androgynous, cityRules.ruleSet, part == first)
          val declinedByCityRule = cityRule.map(rule => applyRule(rule, part, toCase))
          declinedByCityRule.orElse(Decliner.declineFirstName(part, toCase))
        }
        sequence(parts.map(p => if (isFrozenPart(p, parts)) Some(p) else declinePart(p))).map(_.mkString)
    }
  }
  private def isFrozenPart(part: String, parts: List[String]): Boolean =
    isFrozen(part, cityRules.frozenParts) || 
      parts.takeWhile(_ != part).exists(p => isFrozen(p, cityRules.frozenPartsAfter))
  private def isFrozen(str: String, words: List[String]): Boolean = words.contains(str.toLowerCase)
}
