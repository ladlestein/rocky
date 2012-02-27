package controllers

import play.api._
import play.api.mvc._
import play.api.templates._
import minerals._

object Application extends Controller {

    val minerals = (real db).minerals

    def index = Action {
        Ok(views.html.index("Your new application is ready.", minerals.toSeq.sortWith(_.chemistry.formula.complexity < _.chemistry.formula.complexity)))
    }

    def render(crystalForm: Option[MineralCrystalForm]) =
        Html(crystalForm match {
            case Some(form) => form.spaceGroup.order.toString
            case _ => ""
    })
    
    def render(f: Formula): Html = {
        val terms = f.terms.map(render(_)).reduceLeft(_ + _)
        val water = Html(if (f.waterQuantity > 0) "·" + f.waterQuantity + "H2O" else "")
        terms + water
    }

    def render(qt: QuantifiedTerm): Html = {
        new Html(render(qt.term).toString + (if (qt.quantity != 1) "<sub>" + qt.quantity.toString + "</sub>" else ""))
    }

    val comma = new Html(",")

    def oxyFormat(number: Int) = "<sup>" + number.abs.toInt + (if (number < 0) "-" else "+") + "</sup>"

    def render(t: Term): Html = {
        new Html(
            t match {
                case t: ElementalTerm =>
                    t.element.symbol + (t.oxidationNumber match {
                        case Some(x) => oxyFormat(x)
                        case None => ""
                    })
                case t: FunctionalGroup => "(" + t.terms.map {
                    render(_)
                }.reduceLeft(_ + _) + ")"
                case t: SubsitutionGroup => "(" + t.terms.map {
                    render(_)
                }.reduceLeft(_ + comma + _) + ")"
            }
        )
    }

}