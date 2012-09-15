package minerals

import com.nowanswers.chemistry.Formula


case class MineralChemistry(name: String, formula: Formula)

trait ChemistryComponent {

    val mineralParser: Chemistry

    trait Chemistry {
        def read: Iterable[MineralChemistry]
    }

}

trait RealChemistryComponent extends ChemistryComponent {

    self: ConfigurationComponent =>

    lazy val mineralParser = new Chemistry with FormulaParser {

        val mineral_name_simple = "[A-Z][a-z]+" r

        def suffix = ("-(" ~ SYMBOL ~ ")") ^^ {
            case m ~ n ~ o => m + n + o
        }

        def mineral_name = (mineral_name_simple ~ opt(suffix)) ^^ {
            case m ~ None => m
            case m ~ Some(s) => m + s
        }

        def quoted[T](x: Parser[T]) = "\"" ~> x <~ "\""

        val remainder = """[^\r\n]*""" r

        def line = quoted(mineral_name) ~ "," ~ quoted(formula) <~ remainder ^^ {
            case name ~ comma ~ formula => MineralChemistry(name, formula)
        }

        def read: Iterable[MineralChemistry] = {
            val source = configuration chemistrySource

            source.getLines().map {
                parse(line, _)
            }.filter {
                _.successful
            }.map {
                _.get
            }.toIterable

        }

    }

}




// TODO
//
// check ("He[S_2_O_6_]", formula, Formula(List(...))) - write a test for functional groups delineated with [ and ]. Can use Parser.into(..) to implement the functionality.
// write a test for non-stoichiometric formulas.
// check ("\"Simple\",\"Fe\",\"\"", line, "") // One string with both columns absent.

