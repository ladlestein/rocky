package minerals

import io.Source
import play.api.Play
import play.api.Play.current
import java.io.File

/**
 * Created by IntelliJ IDEA.
 * User: larry
 * Date: 2/25/12
 * Time: 9:04 PM
 * To change this template use File | Settings | File Templates.
 */

trait ConfigurationComponent {

    val configuration: Configuration

    trait Configuration {
        
        def groupSource: Source
        
        def formSource: Source

        def chemistrySource: Source

    }

}

trait RealConfigurationComponent extends ConfigurationComponent {

    lazy val configuration = new Configuration {

        lazy val chemistrySource = {
            val chemFilename = Play.configuration.getString("chemistryFile").get
            Source.fromFile(new File(chemFilename))
        }

        lazy val formSource = {
            val formFilename = Play.configuration.getString("formsFile").get
            Source.fromFile(new File(formFilename))
        }
        
        lazy val groupSource = {
            val groupFilename = Play.configuration.getString("groupsFile").get
            Source.fromFile(new File(groupFilename))
        }
    }
}