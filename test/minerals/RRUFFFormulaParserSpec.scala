package minerals

import org.specs2.mutable._
import com.nowanswers.chemistry._
import com.nowanswers.chemistry.Formula
import com.nowanswers.chemistry.Element
import com.nowanswers.chemistry.QuantifiedTerm


class RRUFFFormulaParserSpec
  extends Specification {

  val parser: RRUFFFormulaParser with Object = new RRUFFFormulaParser {}
  def parseFormula(formulaText: String) = parser.parse(parser.formula, formulaText).get
  def parseElement(elementText: String) = parser.parse(parser.element, elementText).get


  val oxygen = Element("O")
  val hydrogen = Element("H")

  "The element list" should {
    "not be null" in {
      Elements.all must not beNull
    }
    "contain an element with a single-character symbol" in {
      Elements.all must contain (oxygen)
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
    "parse a formula with an oxidation number" in {
      parseFormula("Fe^3+^") must_== (Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe"), Option(3))))))
    }
    "parse a formula with a negative oxidation number" in {
      parseFormula("Br^3-^") must_== (Formula(List(QuantifiedTerm(ElementalTerm(Element("Br"), Option(-3))))))
    }
    "parse a formula with multiple terms" in {
      parseFormula("OH") must_== (Formula(List(QuantifiedTerm(ElementalTerm(oxygen)), QuantifiedTerm(ElementalTerm(hydrogen)))))
    }
    "parse a formula with a functional group" in {
      parseFormula("H(SO_4_)") must_== (Formula(List(QuantifiedTerm(ElementalTerm(Element("H"))), QuantifiedTerm(FunctionalGroup(List(QuantifiedTerm(ElementalTerm(Element("S"))), QuantifiedTerm(ElementalTerm(Element("O")), 4)))))))
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