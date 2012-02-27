package minerals

import org.specs2.mutable._


class FormulaParserSpec extends Specification {

  val parser = new FormulaParser {}

  def parseFormula(formula: String) = parser.parse(parser.formula, formula).get
  def parseElement(element: String) = parser.parse(parser.element, element).get

  "The element list" should {
      "not be null" in {
          Elements.all must not beNull
      }
  }
  
  "The element list" should {
      "contain an element with a single-character symbol" in {
          Elements.all must contain (Element("O"))
      }
  }

  "The element list" should {
      "contain an element with a double-character symbol" in {
          Elements.all must contain (Element("Fe"))
      }
  }

  "The element list" should {
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
  }

  "A formula parser" should {
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