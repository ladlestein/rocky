package minerals

import collection.Seq

/**
 * Created by IntelliJ IDEA.
 * User: larry
 * Date: 2/25/12
 * Time: 8:55 PM
 * To change this template use File | Settings | File Templates.
 */

trait MineralDatabaseComponent {

    def minerals: MineralDatabase

    trait MineralDatabase

}

trait RealMineralDatabaseComponent extends MineralDatabaseComponent {
    self: ChemistryComponent =>

    val minerals = new MineralDatabase {
        val chemistry: Seq[MineralChemistry] = mineralParser.read.toSeq.sortBy(x => (x.formula.terms.length))

    }
}



