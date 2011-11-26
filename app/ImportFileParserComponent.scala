package minerals

import scala.util.parsing.combinator.RegexParsers
import scala.io.Source
import java.io.File

case class Mineral(name: String, formula: Formula)

trait ImportFileParserComponent {
  val parser: ImportFileParser

  class ImportFileParser extends FormulaParser {


    val mineral_name_simple = "[A-Z][a-z]+"r

    def suffix = ("-(" ~ SYMBOL ~ ")") ^^ {
      case m ~ n ~ o => m + n + o
    }

    def mineral_name = (mineral_name_simple ~ opt(suffix)) ^^ {
      case m ~ None => m
      case m ~ Some(s) => m + s
    }

    def quoted[T](x: Parser[T]) = "\"" ~> x <~ "\""

    val remainder = """[^\r\n]*"""r

    def line = quoted(mineral_name) ~ "," ~ quoted(formula) <~ remainder ^^ {
      case name ~ comma ~ formula => Mineral(name, formula)
    }

    def read(source: Source) = {
      source.getLines().map {
        parse(line, _) 
      }.filter {_.successful}.map {_.get}
      
    }

  }

}

object Main extends ImportFileParserComponent {
  val parser = new ImportFileParser
  
  def main(args: Array[String]) {
    val inputfile = scala.io.Source.fromFile(new java.io.File("/home/ladlestein/data/RRUFF_Export_20111104_122649.csv"))
    val items = parser.read(inputfile)
    for (item <- items) {
      println(item)
    }
  }

}


// TODO
//
// check ("He[S_2_O_6_]", formula, Formula(List(...))) - write a test for functional groups delineated with [ and ]. Can use Parser.into(..) to implement the functionality.
// write a test for non-stoichiometric formulas.
// check ("\"Simple\",\"Fe\",\"\"", line, "") // One string with both columns absent.

