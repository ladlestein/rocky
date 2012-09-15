package minerals

import org.specs2.mutable._
import com.nowanswers.chemistry._
import com.nowanswers.chemistry.Formula
import com.nowanswers.chemistry.Element
import com.nowanswers.chemistry.QuantifiedTerm


class FormulaParserSpec extends Specification {

  val parser = new FormulaParser {}

  def parseFormula(formula: String) = parser.parse(parser.formula, formula).get
  def parseElement(element: String) = parser.parse(parser.element, element).get

  "The element list" should {
    "not be null" in {
      Elements.all must not beNull
    }
    "contain an element with a single-character symbol" in {
      Elements.all must contain (Element("O"))
    }
    "contain an element with a double-character symbol" in {
      Elements.all must contain (Element("Fe"))
    }
    "not contain an element that doesn't exist" in {
      Elements.all must not contain (Element("X"))
    }
  }

  "An element parser" should {
    "parse an element" in {
        parseElement("Fe") must_== (Element("Fe"))
    }
  }

  "A formula parser" should {
    "parse a single-element formula" in {
          parseFormula("Fe") must_== (Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe"))))))
    }
    "parse a formula with a quantified element" in {
      parseFormula("Fe_2_") must_== (Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe")), 2))))
    }
  }

  //   def test = {
  //
  //      def check[T](s: String, parser: => Parser[AnyRef], expect: AnyRef): AnyRef = {
  //        println(expect)
  //        val result = parse(parser, s).get
  //        assert(result.toString == expect.toString, "expected: \n" + expect + "; got: \n" + result)
  //        result
  //      }
  //      check("Fe", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe"))))))
  //      check("Fe_2_", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe")), 2))))
  //      check("Fe^2+^", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe"), Option(2))))))
  //      check("Si^4-^", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("Si"), Option(-4))))))
  //      check("OH", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("O"))), QuantifiedTerm(ElementalTerm(Element("H"))))))
  //      check("H(SO_4_)", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("H"))), QuantifiedTerm(FunctionalGroup(List(QuantifiedTerm(ElementalTerm(Element("S"))), QuantifiedTerm(ElementalTerm(Element("O")), 4)))))))
  //
  //    }
  //

}