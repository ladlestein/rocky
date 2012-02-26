package minerals

import io.Source
import play.Play
import java.io.File

/**
 * Created by IntelliJ IDEA.
 * User: larry
 * Date: 2/25/12
 * Time: 9:04 PM
 * To change this template use File | Settings | File Templates.
 */

trait ConfigurationComponent {

    def configuration: Configuration

    trait Configuration {
        
        def formSource: Source

        def chemistrySource: Source

    }

}

trait RealConfigurationComponent extends ConfigurationComponent {

    val configuration = new Configuration {

        val chemistrySource = {
            val chemFilename = Play.configuration.getProperty("chemistryFile")
            Source.fromFile(new File(chemFilename))
        }

        def formSource = {
            val formFilename = Play.configuration.getProperty("formsFile")
            Source.fromFile(new File(formFilename))
        }
    }
}