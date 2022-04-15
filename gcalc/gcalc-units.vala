/* gcalc-units.vala
 *
 * Copyright (C) 2022  Daniel Espinosa <esodan@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 *
 * Authors:
 *      Daniel Espinosa <esodan@gmail.com>
 */

public enum GCalc.AngleUnit
{
    RADIANS,
    DEGREES,
    GRADIANS
}

public class GCalc.UnitConverter : GLib.Object {
    public static MathConstant angle (MathConstant c, AngleUnit ori, AngleUnit dst) {
        Constant rc = new Constant.assign (c);
        if (ori == dst) {
            return rc;
        }

        var pi = Calculator.pi ();
        if (Calculator.gt (rc, pi.multiply (new Constant.@double (2.0)))) {
            var t = rc.divide (pi);
            rc = (Constant) rc.subtract (pi.multiply (t));
        }


        if (ori == AngleUnit.DEGREES && dst == AngleUnit.RADIANS) {
            var d = new Constant.@double (180.0);
            var g = pi.divide (d);
            return rc.multiply (g);
        }

        if (ori == AngleUnit.GRADIANS && dst == AngleUnit.RADIANS) {
            var d = new Constant.@double (400.0);
            var grd = pi.divide (d);
            return rc.multiply (grd);
        }

        if (ori == AngleUnit.DEGREES && dst == AngleUnit.GRADIANS) {
            var d = new Constant.@double (9.0);
            var g = new Constant.@double (10.0);
            var grd = g.divide (d);
            return rc.multiply (grd);
        }

        if (ori == AngleUnit.GRADIANS && dst == AngleUnit.DEGREES) {
            var d = new Constant.@double (9.0);
            var g = new Constant.@double (10.0);
            var dg = d.divide (g);
            return rc.multiply (dg);
        }

        return rc;
    }
}
