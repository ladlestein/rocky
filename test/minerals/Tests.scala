package minerals

import play._
import play.test._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.matchers._

@RunWith(classOf[JUnitRunner])
class FormulaParserSpec extends UnitFlatSpec with ShouldMatchers with OneInstancePerTest {

  val parser = new FormulaParser

  def parseFormula(formula: String) = parser.parse(parser.formula, formula).get
  def parseElement(element: String) = parser.parse(parser.element, element).get

  "The element list" should "not be null" in {
    assert(Elements.all != null)
  }
  
  "The element list" should "contain an element with a single-character symbol" in {
    assert(Elements.all.contains(Element("O")))
  }

  "The element list" should "contain an element with a double-character symbol" in {
    assert(Elements.all.contains(Element("Fe")))
  }

  "The element list" should "not contain an element that doesn't exist" in {
    assert(! Elements.all.contains(Element("X")))
  }
  
  "An element parser" should "parse an element" in {
    assert(parseElement("Fe") == Element("Fe"))
  }

  "A formula parser" should "parse a single-element formula" in {
    println(parseFormula("Fe"))
    assert(parseFormula("Fe") == Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe"))))))
  }

  "A formula parser" should "parse a formula with a quantified element" in {
    assert(parseFormula("Fe_2_") == Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe")), 2))))
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