package minerals

import collection.Seq
import util.continuations._

/**
 * Created by IntelliJ IDEA.
 * User: larry
 * Date: 2/25/12
 * Time: 8:55 PM
 * To change this template use File | Settings | File Templates.
 */


case class Mineral(name: String, chemistry: MineralChemistry, form: Option[MineralCrystalForm])

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
        
        lazy val minerals = {
            val f = new Finder
            lazy val chemistry: Seq[MineralChemistry] = mineralParser.read.toSeq.sortBy(x => x.name)
            chemistry.map { chem => Mineral(chem.name, chem, f.find(chem.name))}
        }

        
    }
}


