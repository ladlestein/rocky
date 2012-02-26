package minerals

import minerals.{RealChemistryComponent, RealMineralDatabaseComponent}


/**
 * Created by IntelliJ IDEA.
 * User: larry
 * Date: 2/1/12
 * Time: 3:14 PM
 *
 * This object is like a Spr*ng context; it contains all the components you need for the app,
 * wired together.
 *
 * Each top-level object in the application will get any dependencies it needs from this object.
 */

object real extends stuff
    with RealMineralDatabaseComponent
    with RealChemistryComponent
    with RealConfigurationComponent





trait stuff