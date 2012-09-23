package minerals

import util.parsing.combinator.RegexParsers
import com.nowanswers.chemistry._
import com.nowanswers.chemistry.Element
import com.nowanswers.chemistry.SubsitutionGroup
import com.nowanswers.chemistry.ElementalTerm
import com.nowanswers.chemistry.FunctionalGroup


trait RRUFFFormulaParser extends FormulaParser {

  def charge = "^" ~> INT ~ SIGN <~ "^" ^^ {
    case value ~ "-" => -value.toInt
    case value ~ "+" => value.toInt
  }

  def quantifier = "_" ~> INT <~ "_" ^^ {
    int => int.toInt
  }

  def element = SYMBOL ^^ {
    x => Element(x.toString)
  }

  def elementalTerm =  element ~ (charge ?) ^^ {
    case sym ~ num => ElementalTerm(sym, num)
  }

  def substitutionGroup = "(" ~> rep1sep(term, ",") <~ ")" ^^ {
    terms => SubsitutionGroup(terms)
  }

  def functionalGroup = "(" ~> rep1(quantifiedTerm) <~ ")" ^^ {
    terms => FunctionalGroup(terms)
  }

  def term = elementalTerm | substitutionGroup | functionalGroup

  def quantifiedTerm = (term ~ (quantifier ?)) ^^ {
    case t ~ q => QuantifiedTerm(t, q.getOrElse(1).asInstanceOf[Double])
  }

  def molecularWater = "·" ~> INT <~ "H_2_O" ^^ {
    int => int.toInt
  }

  def formula = (rep1(quantifiedTerm) ~ (molecularWater ?)) ^^ {
    case terms ~ q => Formula(terms, q.getOrElse(0))
  }

}
