package controllers

import play._
import play.mvc._
import minerals._
import play.templates.Html

object Application extends Controller with ImportFileParserComponent {
    
    import views.Application._
    
    val parser = new ImportFileParser
    val inputfilename = Play.configuration.getProperty("inputfilename")
    val inputfile = scala.io.Source.fromFile(new java.io.File(inputfilename))
    val minerals = parser.read ( inputfile ).toSeq.sortBy(x => (x.formula.terms.length))
    
    def index = {
        html.index("Your Scala application is ready!", minerals)
    }
    
	def render(f: Formula): Html = {
			new Html(
			    f.terms.map( render(_)).reduceLeft(_ + _) 
			    + (if (f.waterQuantity > 0) "·" + f.waterQuantity + "H2O" else ""))
	}
	
	def render(qt: QuantifiedTerm) : Html = {
	  new Html( render(qt.term).toString + (if (qt.quantity != 1) "<sub>" + qt.quantity.toString  + "</sub>" else "") )
	}
	
	val comma = new Html(",")
	
	def oxyFormat(number: Int) = "<sup>" + number.abs.toInt + (if (number < 0) "-" else "+") + "</sup>"
	
	def render(t: Term) : Html = {
	  new Html(
		  t match {
		    case t:ElementalTerm => 
		      	t.element.symbol + (t.oxidationNumber match {case Some(x) => oxyFormat(x) case None => ""})
		    case t:FunctionalGroup => "(" + t.terms.map {render(_)}.reduceLeft(_ + _) + ")"
		    case t:SubsitutionGroup => "(" + t.terms.map {render(_)}.reduceLeft(_ + comma + _) + ")"
		  }
	  )
	}
}
