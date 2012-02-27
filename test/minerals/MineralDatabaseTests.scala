package minerals

import io.Source
import org.specs2.mutable._
import org.specs2.mock.Mockito
import org.specs2.specification.Scope

/**
 * Created by IntelliJ IDEA.
 * User: larry
 * Date: 2/25/12
 * Time: 9:49 PM
 * To change this template use File | Settings | File Templates.
 */

class MineralDatabaseTests extends Specification {

    trait Fixture
        extends RealMineralDatabaseComponent
        with ChemistryComponent
        with CrystallographyComponent
        with ConfigurationComponent
        with Mockito
        with Scope {

        val mineralParser = mock[Chemistry]
        val formParser = mock[Crystallography]
        val configuration = mock[Configuration]

    }

    "A mineral form specification" should {
        "be parsed" in new Fixture {

            (formParser forms) returns List(
                MineralCrystalForm("Larryite", SpaceGroup(227, 192, "Oh^7", "Fd-3m:1", "m-3m" )),
                MineralCrystalForm("Edelsteinite", SpaceGroup(26, 12, "fake", "fake", "fakefake" ))
            )

            (mineralParser read) returns List[MineralChemistry](
                MineralChemistry("Larryite", new Formula(List(), 0)),
                MineralChemistry("Edelsteinite", new Formula(List(), 0))
            )

            (db minerals) must_== (Seq(
                Mineral("Edelsteinite",
                    MineralChemistry("Edelsteinite", new Formula(List(), 0)),
                    Some(MineralCrystalForm("Edelsteinite", SpaceGroup(26, 12, "fake", "fake", "fakefake" )))
                ),
                    Mineral("Larryite",
                        MineralChemistry("Larryite", new Formula(List(), 0)),
                        Some(MineralCrystalForm("Larryite", SpaceGroup(227, 192, "Oh^7", "Fd-3m:1", "m-3m")))
                )
            ))
        }
    }

}


