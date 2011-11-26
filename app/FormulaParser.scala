package minerals

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

object Elements {
  
  val raw = """
  H He 
  Li Be B C N O F Ne 
  Na Mg Al Si P S Cl Ar 
  K Ca Sc Ti V Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr 
  Rb Sr Y Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I Xe
  Cs Ba La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu Hf Ta W Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn
  Fr Ra Ac Th Pa U Np Pu Am Cm Bk Cf Es Fm Md No Lr Rf Db Sg Bh Hs Mt Ds Rg Cn Uut Uuq Uup Uuh Uus Uuo
  REE
"""

  val all = new Regex("[A-Za-z]+")
    .findAllIn(raw).map { Element(_) }.toSeq

}

case class Element(symbol: String)
case class ElementalTerm(element: Element, oxidationNumber: Option[Int] = None) extends Term
case class SubsitutionGroup(terms: List[Term]) extends Term
case class FunctionalGroup(terms: List[QuantifiedTerm]) extends Term
abstract class Term

case class QuantifiedTerm(term: Term, quantity: Int = 1)
case class Formula(terms: List[QuantifiedTerm], waterQuantity: Int = 0) {
  def complexity = terms.length
}

class FormulaParser extends RegexParsers {

  val INT = """[1-9][0-9]*"""r

  val NEWLINE = """\r?\n"""r

  val SIGN = """[+-]"""r

  val SYMBOL = """(REE)|([A-Z][a-z]?)"""r

  def oxidationNumber = "^" ~> INT ~ SIGN <~ "^" ^^ {
    case value ~ "-" => -value.toInt
    case value ~ "+" => value.toInt
  }

  def quantifier = "_" ~> INT <~ "_" ^^ {
    int => int.toInt
  }

  def element = SYMBOL ^^ {
    x => Element(x.toString)
  }
    
  def elementalTerm =  element ~ (oxidationNumber ?) ^^ {
    case sym ~ num => ElementalTerm(sym, num)
  }

  def substitutionGroup = "(" ~> rep1sep(term, ",") <~ ")" ^^ {
    terms => SubsitutionGroup(terms)
  }

  def functionalGroup = "(" ~> rep1(quantifiedTerm) <~ ")" ^^ {
    terms => FunctionalGroup(terms)
  }

  def term: Parser[Term] = elementalTerm | substitutionGroup | functionalGroup

  def quantifiedTerm: Parser[QuantifiedTerm] = (term ~ (quantifier ?)) ^^ {
    case t ~ q => QuantifiedTerm(t, q.getOrElse(1))
  }

  def molecularWater = "·" ~> INT <~ "H_2_O" ^^ {
    int => int.toInt
  }

  def formula = (rep1(quantifiedTerm) ~ (molecularWater ?)) ^^ {
    case terms ~ q => Formula(terms, q.getOrElse(0))
  }

}