package minerals

import io.Source
import play.api.libs.json._

/**
 * Created by IntelliJ IDEA.
 * User: larry
 * Date: 2/25/12
 * Time: 9:22 PM
 * To change this template use File | Settings | File Templates.
 */



case class MineralCrystalForm(name: String, spaceGroup: SpaceGroup)
case class SpaceGroup(number: Int, order: Int, schoenflies: String, hm: String, pointGroup: String)

trait CrystallographyComponent {

    val formParser: Crystallography

    trait Crystallography {

        val forms: List[MineralCrystalForm]
        val groups: List[SpaceGroup]
    }

    implicit object MineralCrystalFormFormat extends Format[MineralCrystalForm] {

        val SgString = """(\d+).*"""r
        def reads(json: JsValue): MineralCrystalForm =
            {
                val sgString: String = (json \ "spaceGroupNumber").as[String]
                val SgString(spaceGroupNumber) = sgString
                MineralCrystalForm(
                    (json \ "name").as[String],
                    formParser.groups.find { _.number == spaceGroupNumber.toInt }.get
                )
            }

        def writes(o: MineralCrystalForm) = null
    }

    implicit object SpaceGroupFormat extends Format[SpaceGroup] {
        def reads(json: JsValue) = {
            SpaceGroup(
                (json \ "number").as[Int],

                (json \ "order").as[Int],
                (json \ "schoenflies").as[String],
                (json \ "hm").as[String],
                (json \ "pointGroup").as[String]
            )
        }

        def writes(o: SpaceGroup) = null
    }

}


trait RealCrystallographyComponent extends CrystallographyComponent {

    self: ConfigurationComponent =>

    lazy val formParser = new Crystallography {

        lazy val forms: List[MineralCrystalForm] = {
            val source: Source = configuration.formSource
            val json: String = source.mkString
            source.close

            Json.parse(json).as[List[MineralCrystalForm]]
        }

        lazy val groups: List[SpaceGroup] = {
            val source: Source = configuration.groupSource
            val json: String = source.mkString
            source.close

            Json.parse(json).as[List[SpaceGroup]]
        }

    }
}