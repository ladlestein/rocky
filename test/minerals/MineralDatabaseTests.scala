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

    implicit def leftToString(l: Left): String = l.name
    implicit def rightToString(r: Right): String = r.name
    case class Left(name: String, size: Int)
    case class Right(name: String, color: String)
    case class Prod(name: String, size: Int, color: Option[String])
    
    val lefts = List(Left("bar", 5), Left("foo", 7), Left("zebra", 27))
    val rights = List(Right("bar", "purple"), Right("zebra", "blue"), Right("zoo", "gray"))

    "A collation of two things" should {
        "work" in {
            (lefts <=> rights) { (left, right) => {

                Prod(left.name, left.size, right flatMap {r => Some(r.color)})

            }} must_== List(("bar", "bar"))
        }
    }

    final class Collation[L](val __leftOfCollate: Iterable[L]) {
        @inline def <=>[R, K, P]
        ( right: Iterable[R] )
            ( combine: (L, Option[R]) => P )
            ( implicit lToKey: L => K,  rToKey: R => K, order: Ordering[K] ) {
            val r = right.iterator.buffered

            val left = __leftOfCollate
            left.map { l =>

                val optionr: Option[R] = {
                    while (r.hasNext && (order.lt(rToKey(r.head), lToKey(l)))) {
                        r.next
                    }

                    if (r.hasNext && order.eq(rToKey(r.head), lToKey(l))) {
                        Some(r.head)
                    } else {
                        None
                    }
                }
                combine(l, optionr)

            }
        }

        //        def â[R](y: R): Tuple2[L, R] = <=>(y)

    }

    implicit def any2Collation[A](x: Iterable[A]): Collation[A] = new Collation(x)

}


