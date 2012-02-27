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

class CrystalFormParserTests extends Specification {

    trait Fixture
        extends RealCrystallographyComponent
        with ConfigurationComponent
        with Mockito
        with Scope {

        val configuration = mock[Configuration]

        configuration.groupSource returns (Source fromString
            """
               [{
                   "number": 227,
                   "order": 192,
                   "schoenflies": "Oh^7",
                   "hm": "Fd-3m:1",
                   "pointGroup": "m-3m"
                },
                {
                    "number":26,
                    "order": 12,
                    "schoenflies": "fake",
                    "hm": "fake",
                    "pointGroup": "fakefake"
                }
                ]
            """
            )

        configuration.formSource returns (Source fromString
            """
                [
                    {
                        "spaceGroupNumber": "227",
                        "name": "Larryite"
                    },
                    {
                        "spaceGroupNumber": "26",
                        "name": "Edelsteinite"
                    }
                ]
            """
            )

    }

    "A space group description" should {
        "be parsed" in new Fixture {

            (formParser groups) must_== (List(
                SpaceGroup(227, 192, "Oh^7", "Fd-3m:1", "m-3m" ),
                SpaceGroup(26, 12, "fake", "fake", "fakefake" )
            ))

        }
    }


    "A mineral form specification" should {
        "be parsed" in new Fixture {

            (formParser forms) must_== (List(
                MineralCrystalForm("Larryite", SpaceGroup(227, 192, "Oh^7", "Fd-3m:1", "m-3m" )),
                MineralCrystalForm("Edelsteinite", SpaceGroup(26, 12, "fake", "fake", "fakefake" ))
            ))

        }
    }

}


