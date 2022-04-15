/* gcalc-calculator.vala
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
/**
 * A constant calculator for different operations.
 */
public class GCalc.Calculator : GLib.Object {
    public static MathConstant add (MathConstant c1, MathConstant c2) {
        var c = new Constant.assign (c1);
        return c.add (c2);
    }
    /**
     * Substract C2 to C1 operation (c1 - c2)
     */
    public static MathConstant subtract (MathConstant c1, MathConstant c2) {
        var c = new Constant.assign (c1);
        return c.subtract (c2);
    }
    public static MathConstant multiply (MathConstant c1, MathConstant c2) {
        var c = new Constant.assign (c1);
        return c.multiply (c2);
    }
    /**
     * Divide c1/c2
     */
    public static MathConstant divide (MathConstant c1, MathConstant c2) {
        var c = new Constant.assign (c1);
        c.divide (c2);
        return c;
    }
    /**
     * Calculates the negative value of c
     */
    public static MathConstant neg (MathConstant c) {
        var rc = new Constant.assign (c);
        return rc.neg ();
    }
    /**
     * Calculates c raised to p
     */
    public static MathConstant pow (MathConstant c, MathConstant p) {
        var rc = new Constant.assign (c);
        return rc.pow (p);
    }
    /**
     * Calculates square root of c
     */
    public static MathConstant sqrt (MathConstant c) {
        var rc = new Constant.assign (c);
        rc.get_complex ().get_real ().val.sqrt (rc.get_complex ().get_real ().val);
        return rc;
    }
    /**
     * Creates a constant with the value of constant PI
     */
    public static MathConstant pi () {
        var rc = new Constant ();
        rc.get_complex ().get_real ().val.const_pi ();
        return rc;
    }
    /**
     * Creates a constant with the value of constant logarithm of 2
     */
    public static MathConstant log2 () {
        var rc = new Constant ();
        rc.get_complex ().get_real ().val.const_log2 ();
        return rc;
    }
    /**
     * Creates a constant with the value of constant EULER
     */
    public static MathConstant euler () {
        var rc = new Constant ();
        rc.get_complex ().get_real ().val.const_euler ();
        return rc;
    }
    /**
     * Creates a constant with the value of constant CATALAN
     */
    public static MathConstant catalan () {
        var rc = new Constant ();
        rc.get_complex ().get_real ().val.const_catalan ();
        return rc;
    }
    /**
     * Operator c1 = c2 (equal)
     */
    public static bool eq (MathConstant c1, MathConstant c2) {
        if (!(c1 is Constant) || !(c2 is Constant)) {
            return false;
        }

        return ((Constant) c1).get_complex ().cmp (((Constant) c2).get_complex ()) == 0 ? true : false;
    }
    /**
     * Operator c1 > c2
     */
    public static bool gt (MathConstant c1, MathConstant c2) {
        if (!(c1 is Constant) || !(c2 is Constant)) {
            return false;
        }

        return ((Constant) c1).get_complex ().cmp (((Constant) c2).get_complex ()) > 0 ? true : false;
    }
    /**
     * Operator c1 < c2
     */
    public static bool lt (MathConstant c1, MathConstant c2) {
        if (!(c1 is Constant) || !(c2 is Constant)) {
            return false;
        }

        return ((Constant) c1).get_complex ().cmp (((Constant) c2).get_complex ()) < 0 ? true : false;
    }
    /**
     * Calculates the cosine of c1 angle at the given units
     */
    public static MathConstant cos (MathConstant c1, GCalc.AngleUnit units = GCalc.AngleUnit.RADIANS) {
        var a = new Constant.assign (c1);
        if (units != GCalc.AngleUnit.RADIANS) {
            a = (Constant) UnitConverter.angle (c1, units, GCalc.AngleUnit.RADIANS);
        }

        var rc = new Constant ();
        rc.get_complex ().get_real ().val.cos (a.get_complex ().get_real ().val);
        return rc;
    }
    /**
     * Calculates the sine of c1 angle at the given units
     */
    public static MathConstant sin (MathConstant c1, GCalc.AngleUnit units = GCalc.AngleUnit.RADIANS) {
        var a = new Constant.assign (c1);
        if (units != GCalc.AngleUnit.RADIANS) {
            a = (Constant) UnitConverter.angle (c1, units, GCalc.AngleUnit.RADIANS);
        }

        var rc = new Constant ();
        rc.get_complex ().get_real ().val.sin (a.get_complex ().get_real ().val);
        return rc;
    }
    /**
     * Calculates the tangent of c1 angle at the given units
     */
    public static MathConstant tan (MathConstant c1, GCalc.AngleUnit units = GCalc.AngleUnit.RADIANS) {
        var a = new Constant.assign (c1);
        if (units != GCalc.AngleUnit.RADIANS) {
            a = (Constant) UnitConverter.angle (c1, units, GCalc.AngleUnit.RADIANS);
        }

        var rc = new Constant ();
        rc.get_complex ().get_real ().val.tan (a.get_complex ().get_real ().val);
        return rc;
    }
    /**
     * Calculates the arc, in the given units, represented by the cosine of c1
     */
    public static MathConstant acos (MathConstant c1, GCalc.AngleUnit units = GCalc.AngleUnit.RADIANS) {
        if (!(c1 is Constant)) {
            return new Constant  ();
        }

        var rc = new Constant ();
        rc.get_complex ().get_real ().val.acos (((Constant) c1).get_complex ().get_real ().val);

        if (units != GCalc.AngleUnit.RADIANS) {
            return (Constant) UnitConverter.angle (rc, GCalc.AngleUnit.RADIANS, units);
        }

        return rc;
    }
    /**
     * Calculates the arc, in the given units, represented by the sine of c1
     */
    public static MathConstant asin (MathConstant c1, GCalc.AngleUnit units = GCalc.AngleUnit.RADIANS) {
        if (!(c1 is Constant)) {
            return new Constant  ();
        }

        var rc = new Constant ();
        rc.get_complex ().get_real ().val.acos (((Constant) c1).get_complex ().get_real ().val);

        if (units != GCalc.AngleUnit.RADIANS) {
            return (Constant) UnitConverter.angle (rc, GCalc.AngleUnit.RADIANS, units);
        }

        return rc;
    }
    /**
     * Calculates the arc, with the given units, represented by the tangent of c1
     */
    public static MathConstant atan (MathConstant c1, GCalc.AngleUnit units = GCalc.AngleUnit.RADIANS) {
        if (!(c1 is Constant)) {
            return new Constant  ();
        }

        var rc = new Constant ();
        rc.get_complex ().get_real ().val.atan (((Constant) c1).get_complex ().get_real ().val);

        if (units != GCalc.AngleUnit.RADIANS) {
            return (Constant) UnitConverter.angle (rc, GCalc.AngleUnit.RADIANS, units);
        }

        return rc;
    }
    /**
     * Calculates the logarithm base 10 of given c1
     */
    public static MathConstant log10 (MathConstant c1) {
        var rc = new Constant ();
        rc.get_complex ().get_real ().val.log10 (((Constant) c1).get_complex ().get_real ().val);
        return rc;
    }
    /**
     * Calculates the the exponential of iven c1
     */
    public static MathConstant exp (MathConstant c1) {
        var rc = new Constant ();
        rc.get_complex ().get_real ().val.exp (((Constant) c1).get_complex ().get_real ().val);
        return rc;
    }
}

