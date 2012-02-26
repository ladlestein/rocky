package minerals

/**
 * Created by IntelliJ IDEA.
 * User: larry
 * Date: 2/25/12
 * Time: 9:22 PM
 * To change this template use File | Settings | File Templates.
 */

trait CrystallographyComponent {

    def mineralForms: Crystallography

    trait Crystallography

}


trait RealCrystallographyComponent extends CrystallographyComponent {

    self: ConfigurationComponent =>

    val mineralForms = new Crystallography {
        configuration.formSource
    }
}