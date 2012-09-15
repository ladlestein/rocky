package minerals

import collection.Seq
import play.api.libs.json._

/**
 * Created by IntelliJ IDEA.
 * User: larry
 * Date: 2/25/12
 * Time: 8:55 PM
 * To change this template use File | Settings | File Templates.
 */


case class Mineral(name: String, chemistry: MineralChemistry, form: Option[MineralCrystalForm])

object Implicits {
    implicit object MineralFormat extends Format[Mineral]{
        def reads(json: JsValue) = null

        def writes(m: Mineral) = {
            val complexity: JsNumber = JsNumber(m.chemistry.formula.complexity)
            val order: JsNumber = JsNumber(m.form.get.spaceGroup.order)
            val name: JsString = JsString(m.name)
            JsObject(List(
                "complexity" -> complexity,
                "order" -> order,
                "name" -> name
            ))
        }
    }
}



trait MineralDatabaseComponent {

    val db: MineralDatabase

    trait MineralDatabase {
        val minerals: Seq[Mineral]
    }

}



trait RealMineralDatabaseComponent extends MineralDatabaseComponent {
    self: ChemistryComponent with CrystallographyComponent =>

    lazy val db = new MineralDatabase {

        lazy val forms = formParser.forms.sortBy(x => x.name)

        class Finder {
            val iter = forms.iterator.buffered
            def find(name: String) = {
                while (iter.hasNext && iter.head.name < name) {iter.next}
                
                if (iter.hasNext && iter.head.name == name) {
                    Some(iter.head)
                } else {
                    None
                }
            }            
        }

        lazy private val chemistry: Seq[MineralChemistry] = mineralParser.read.toSeq.sortBy(x => x.name)
        
        lazy val minerals = {
            val f = new Finder
            chemistry.map { chem => Mineral(chem.name, chem, f.find(chem.name))}
        }

//        lazy val newminerals = (chemistry <=> forms) { (chem, form) => Mineral(chem.name, chem, form)}
        
        implicit def chemToKey(chem: MineralChemistry): String = chem.name
        implicit def formToKey(form: MineralCrystalForm): String = form.name
    }


//    final class Collation[L](val __leftOfCollate: Iterable[L]) {
//        @inline def <=>[R, K, P]
//            ( right: Iterable[R] )
//            ( combine: (L, Option[R]) => P )
//            ( implicit lToKey: L => K,  rToKey: R => K, order: Ordering[K] ) {
//            val r = right.iterator.buffered
//
//            val left = __leftOfCollate
//            left.map { l =>
//
//                val optionr: Option[R] = {
//                    while (r.hasNext && (order.lt(rToKey(r.head), lToKey(l)))) {
//                        r.next
//                    }
//
//                    if (r.hasNext && order.eq(rToKey(r.head), lToKey(l))) {
//                        Some(r.head)
//                    } else {
//                        None
//                    }
//                }
//                combine(l, optionr)
//
//            }
//        }
//
////        def â[R](y: R): Tuple2[L, R] = <=>(y)
//
//    }
//
//    implicit def any2Collation[A](x: Iterable[A]): Collation[A] = new Collation(x)

}

