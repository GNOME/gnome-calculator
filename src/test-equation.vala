/*
 * Copyright (C) 2008-2012 Robert Ancell.
 * 
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */
 
private int number_base = 10;
private int wordlen = 32;
private AngleUnit angle_units = AngleUnit.DEGREES;
private bool enable_conversions = false;
private bool enable_variables = false;

private int fail_count = 0;
private int pass_count = 0;

private string error_code_to_string (ErrorCode error)
{
    if (error == ErrorCode.MP)
        return "ErrorCode.MP(\"%s\")".printf (mp_get_error ());

    return mp_error_code_to_string (error);
}

private void test (string expression, string expected, ErrorCode expected_error)
{
    var equation = new TestEquation (expression, enable_variables, enable_conversions);
    equation.base = number_base;
    equation.wordlen = wordlen;
    equation.angle_units = angle_units;

    ErrorCode error;
    uint representation_base;
    var result = equation.parse (out representation_base, out error);

    if (result == null)
    {
        if (error == expected_error)
        {
            /*stdout.printf ("PASS: '%s' -> error %s\n", expression, error_code_to_string (error));*/
            pass_count++;
        }
        else if (expected_error == ErrorCode.NONE)
        {
            stdout.printf ("*FAIL: '%s' -> error %s, expected result %s\n", expression, error_code_to_string (error), expected);
            fail_count++;
        }
        else
        {
            stdout.printf ("*FAIL: '%s' -> error %s, expected error %s\n", expression, error_code_to_string (error), error_code_to_string (expected_error));
            fail_count++;
        }
    }
    else
    {
        var serializer = new Serializer (DisplayFormat.FIXED, number_base, 9);
        serializer.set_representation_base (representation_base);
        var result_str = serializer.to_string (result);

        if (expected_error != ErrorCode.NONE)
        {
            stdout.printf ("*FAIL: '%s' -> %s, expected error %s\n", expression, result_str, error_code_to_string (expected_error));
            fail_count++;
        }
        else if (result_str != expected)
        {
            stdout.printf ("*FAIL: '%s' -> '%s', expected '%s'\n", expression, result_str, expected);
            fail_count++;
        }
        else
        {
            /*stdout.printf ("PASS: '%s' -> '%s'\n", expression, result_str);*/
            pass_count++;
        }
    }
}

private class TestEquation : Equation
{
    private bool enable_variables;
    private bool enable_conversions;

    public TestEquation (string equation, bool enable_variables, bool enable_conversions)
    {
        base (equation);
        this.enable_variables = enable_variables;
        this.enable_conversions = enable_conversions;
    }

    public override bool variable_is_defined (string name)
    {
        if (!enable_variables)
            return false;

        return name == "x" || name == "y";
    }

    public override Number? get_variable (string name)
    {
        if (!enable_variables)
            return null;

        if (name == "x")
            return new Number.integer (2);
        if (name == "y")
            return new Number.integer (3);

        return null;
    }

    public override Number? convert (Number x, string x_units, string z_units)
    {
        if (!enable_conversions)
            return null;
        
        return UnitManager.get_default ().convert_by_symbol (x, x_units, z_units);
    }
}

private void test_conversions ()
{
    number_base = 10;
    wordlen = 32;
    angle_units = AngleUnit.DEGREES;
    enable_conversions = true;
    enable_variables = false;

    /* Angle units */
    //test ("π radians in degrees", "180", 0);
    test ("100 gradians in degrees", "90", 0);

    /* Length */
    test ("1 meter in mm", "1000", 0);  
    test ("1m in mm", "1000", 0);
    test ("1 inch in cm", "2.54", 0);

    /* Area */
    test ("1m² in mm²", "1000000", 0);
  
    /* Volume */
    test ("1m³ in mm³", "1000000000", 0);  

    /* Weight */
    test ("1 kg in pounds", "2.204622622", 0);
  
    /* Duration */
    test ("1 minute in seconds", "60", 0);
    test ("1s in ms", "1000", 0);

    /* Temperature */
    //test ("100˚C in ˚F", "", 0);
    //test ("0˚C in ˚F", "32", 0);
    //test ("0˚K in ˚C", "−273.15", 0);
    test ("100degC in degF", "212", 0);
    test ("0degC in degF", "32", 0);
    test ("0 K in degC", "−273.15", 0);
}

private void test_equations ()
{
    number_base = 10;
    wordlen = 32;
    angle_units = AngleUnit.DEGREES;
    enable_conversions = false;
    enable_variables = true;

    number_base = 2;
    test ("2₁₀", "10", 0);

    number_base = 8;
    test ("16434824₁₀", "76543210", 0);

    number_base = 16;
    test ("FF", "FF", 0);
    test ("18364758544493064720₁₀", "FEDCBA9876543210", 0);

    number_base = 10;
    test ("0₂", "0", 0); test ("0₈", "0", 0); test ("0", "0", 0); test ("0₁₆", "0", 0);
    test ("1₂", "1", 0); test ("1₈", "1", 0); test ("1", "1", 0); test ("1₁₆", "1", 0);
    test ("2₂", "", ErrorCode.INVALID); test ("2₈", "2", 0); test ("2", "2", 0); test ("2₁₆", "2", 0);
    test ("3₂", "", ErrorCode.INVALID); test ("3₈", "3", 0); test ("3", "3", 0); test ("3₁₆", "3", 0);
    test ("4₂", "", ErrorCode.INVALID); test ("4₈", "4", 0); test ("4", "4", 0); test ("4₁₆", "4", 0);
    test ("5₂", "", ErrorCode.INVALID); test ("5₈", "5", 0); test ("5", "5", 0); test ("5₁₆", "5", 0);
    test ("6₂", "", ErrorCode.INVALID); test ("6₈", "6", 0); test ("6", "6", 0); test ("6₁₆", "6", 0);
    test ("7₂", "", ErrorCode.INVALID); test ("7₈", "7", 0); test ("7", "7", 0); test ("7₁₆", "7", 0);
    test ("8₂", "", ErrorCode.INVALID); test ("8₈", "", ErrorCode.INVALID); test ("8", "8", 0); test ("8₁₆", "8", 0);
    test ("9₂", "", ErrorCode.INVALID); test ("9₈", "", ErrorCode.INVALID); test ("9", "9", 0); test ("9₁₆", "9", 0);
    test ("A₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("A₈", "", ErrorCode.UNKNOWN_VARIABLE); test ("A", "", ErrorCode.UNKNOWN_VARIABLE); test ("A₁₆", "10", 0);
    test ("B₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("B₈", "", ErrorCode.UNKNOWN_VARIABLE); test ("B", "", ErrorCode.UNKNOWN_VARIABLE); test ("B₁₆", "11", 0);
    test ("C₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("C₈", "", ErrorCode.UNKNOWN_VARIABLE); test ("C", "", ErrorCode.UNKNOWN_VARIABLE); test ("C₁₆", "12", 0);
    test ("D₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("D₈", "", ErrorCode.UNKNOWN_VARIABLE); test ("D", "", ErrorCode.UNKNOWN_VARIABLE); test ("D₁₆", "13", 0);
    test ("E₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("E₈", "", ErrorCode.UNKNOWN_VARIABLE); test ("E", "", ErrorCode.UNKNOWN_VARIABLE); test ("E₁₆", "14", 0);
    test ("F₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("F₈", "", ErrorCode.UNKNOWN_VARIABLE); test ("F", "", ErrorCode.UNKNOWN_VARIABLE); test ("F₁₆", "15", 0);
    test ("a₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("a₈", "", ErrorCode.UNKNOWN_VARIABLE); test ("a", "", ErrorCode.UNKNOWN_VARIABLE); test ("a₁₆", "10", 0);
    test ("b₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("b₈", "", ErrorCode.UNKNOWN_VARIABLE); test ("b", "", ErrorCode.UNKNOWN_VARIABLE); test ("b₁₆", "11", 0);
    test ("c₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("c₈", "", ErrorCode.UNKNOWN_VARIABLE); test ("c", "", ErrorCode.UNKNOWN_VARIABLE); test ("c₁₆", "12", 0);
    test ("d₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("d₈", "", ErrorCode.UNKNOWN_VARIABLE); test ("d", "", ErrorCode.UNKNOWN_VARIABLE); test ("d₁₆", "13", 0);
    test ("e₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("e₈", "", ErrorCode.UNKNOWN_VARIABLE); /* e is a built-in variable */              test ("e₁₆", "14", 0);
    test ("f₂", "", ErrorCode.UNKNOWN_VARIABLE); test ("f₈", "", ErrorCode.UNKNOWN_VARIABLE); test ("f", "", ErrorCode.UNKNOWN_VARIABLE); test ("f₁₆", "15", 0);

    test ("+1", "1", 0);
    test ("−1", "−1", 0);
    test ("+ 1", "1", 0); // FIXME: Should this be allowed?
    test ("− 1", "−1", 0); // FIXME: Should this be allowed?
    test ("++1", "1", ErrorCode.INVALID);
    test ("−−1", "1", 0);
    test ("255", "255", 0);
    test ("256", "256", 0);
    test ("½", "0.5", 0);
    test ("1½", "1.5", 0);
    test ("0°", "0", 0);
    test ("1°", "1", 0);
    test ("0°30'", "0.5", 0);
    //test ("0°0.1'", "1", 0); // FIXME: Not yet supported
    test ("0°0'1\"", "0.000277778", 0);
    test ("0°0'0.1\"", "0.000027778", 0);
    test ("1.00", "1", 0);
    test ("1.01", "1.01", 0);

    test ("١٢٣٤٥٦٧٨٩٠", "1234567890", 0);
    test ("۱۲۳۴۵۶۷۸۹۰", "1234567890", 0);

/*    
    //test ("2A", "2000000000000000", 0);
    test ("2T", "2000000000000", 0);
    test ("2G", "2000000000", 0);
    test ("2M", "2000000", 0);
    test ("2k", "2000", 0);
    test ("2c", "0.02", 0);
    test ("2d", "0.2", 0);
    test ("2c", "0.02", 0);
    test ("2m", "0.002", 0);
    test ("2u", "0.000002", 0);
    test ("2µ", "0.000002", 0);
    test ("2n", "0.000000002", 0);
    //test ("2p", "0.000000000002", 0); // FIXME: Need to print out significant figures, not decimal places
    //test ("2f", "0.000000000000002", 0); // FIXME: Need to print out significant figures, not decimal places
    //test ("2A3", "2300000000000000", 0);
    test ("2T3", "2300000000000", 0);
    test ("2G3", "2300000000", 0);
    test ("2M3", "2300000", 0);
    test ("2k3", "2300", 0);
    test ("2c3", "0.023", 0);
    test ("2d3", "0.23", 0);
    test ("2c3", "0.023", 0);
    test ("2m3", "0.0023", 0);
    test ("2u3", "0.0000023", 0);
    test ("2µ3", "0.0000023", 0);
    //test ("2n3", "0.0000000023", 0); // FIXME: Need to print out significant figures, not decimal places
    //test ("2p3", "0.0000000000023", 0); // FIXME: Need to print out significant figures, not decimal places
    //test ("2f3", "0.0000000000000023", 0); // FIXME: Need to print out significant figures, not decimal places
*/

    test ("2×10^3", "2000", 0);
    test ("2×10^−3", "0.002", 0);

    test ("x", "2", 0);
    test ("y", "3", 0);
    test ("z", "", ErrorCode.UNKNOWN_VARIABLE);
    test ("2y", "6", 0);
    test ("y2", "", ErrorCode.INVALID);
    test ("y 2", "", ErrorCode.INVALID);
    test ("2z", "", ErrorCode.UNKNOWN_VARIABLE);  
    test ("z2", "", ErrorCode.UNKNOWN_VARIABLE);
    test ("z 2", "", ErrorCode.UNKNOWN_VARIABLE);
    test ("z(2)", "", ErrorCode.UNKNOWN_VARIABLE);
    test ("y²", "9", 0);
    test ("2y²", "18", 0);
    test ("x×y", "6", 0);
    test ("xy", "6", 0);
    test ("yx", "6", 0);
    test ("2xy", "12", 0);
    test ("x²y", "12", 0);
    test ("xy²", "18", 0);
    test ("(xy)²", "36", 0);
    test ("2x²y", "24", 0);
    test ("2xy²", "36", 0);
    test ("2x²y²", "72", 0);
    test ("x²yx²y", "144", 0);
    test ("x³+2x²−5", "11", 0);
    test ("2(x+3y)", "22", 0);
    test ("x(x+3y)", "22", 0);
    test ("(x+3y)(2x-4y)", "−88", 0);
    test ("2x²+2xy−12y²", "−88", 0);

    test ("π", "3.141592654", 0);
    test ("e", "2.718281828", 0);

    test ("z=99", "99", 0);
    test ("longname=99", "99", 0);
    //test ("e=99", "", ErrorCode.BUILTIN_VARIABLE);

    test ("0+0", "0", 0);
    test ("1+1", "2", 0);
    test ("1+4", "5", 0);
    test ("4+1", "5", 0);
    test ("40000+0.001", "40000.001", 0);
    test ("0.001+40000", "40000.001", 0);
    test ("2-3", "−1", 0);
    test ("2−3", "−1", 0);
    test ("3−2", "1", 0);
    test ("40000−0.001", "39999.999", 0);
    test ("0.001−40000", "−39999.999", 0);
    test ("2*3", "6", 0);
    test ("2×3", "6", 0);
    test ("−2×3", "−6", 0);
    test ("2×−3", "−6", 0);
    test ("−2×−3", "6", 0);
    test ("6/3", "2", 0);
    test ("6÷3", "2", 0);
    test ("1÷2", "0.5", 0);
    test ("−6÷3", "−2", 0);
    test ("6÷−3", "−2", 0);
    test ("−6÷−3", "2", 0);
    test ("(−3)÷(−6)", "0.5", 0);
    test ("2÷2", "1", 0);
    test ("1203÷1", "1203", 0);
    test ("−0÷32352.689", "0", 0);
    test ("1÷4", "0.25", 0);
    test ("1÷3", "0.333333333", 0);
    test ("2÷3", "0.666666667", 0);
    test ("1÷0", "", ErrorCode.MP);
    test ("0÷0", "", ErrorCode.MP);

    /* Precision */
    test ("1000000000000000−1000000000000000", "0", 0);
    test ("1000000000000000÷1000000000000000", "1", 0);
    test ("1000000000000000×0.000000000000001", "1", 0);

    /* Order of operations */
    test ("1−0.9−0.1", "0", 0);
    test ("1+2×3", "7", 0);
    test ("1+(2×3)", "7", 0);
    test ("(1+2)×3", "9", 0);
    test ("(1+2×3)", "7", 0);
    test ("2(1+1)", "4", 0);
    test ("4÷2(1+1)", "4", 0);

    /* Percentage */
    test ("100%", "1", 0);
    test ("1%", "0.01", 0);
    test ("100+1%", "101", 0);
    test ("100−1%", "99", 0);
    test ("100×1%", "1", 0);
    test ("100÷1%", "10000", 0);

    /* Factorial */
    test ("0!", "1", 0);
    test ("1!", "1", 0);
    test ("5!", "120", 0);
    test ("69!", "171122452428141311372468338881272839092270544893520369393648040923257279754140647424000000000000000", 0);
    test ("0.1!", "", ErrorCode.MP);
    test ("−1!", "−1", 0);
    test ("(−1)!", "", ErrorCode.MP);
    test ("−(1!)", "−1", 0);

    /* Powers */
    test ("2²", "4", 0);
    test ("2³", "8", 0);
    test ("2¹⁰", "1024", 0);
    test ("(1+2)²", "9", 0);
    test ("(x)²", "4", 0);
    test ("|1−3|²", "4", 0);
    test ("|x|²", "4", 0);
    test ("0^0", "1", 0);
    test ("0^0.5", "0", 0);
    test ("2^0", "1", 0);
    test ("2^1", "2", 0);
    test ("2^2", "4", 0);
    test ("2⁻¹", "0.5", 0);
    test ("2⁻", "", ErrorCode.MP);
    test ("2^−1", "0.5", 0);
    test ("2^(−1)", "0.5", 0);
    test ("x⁻¹", "0.5", 0);
    test ("−10^2", "−100", 0);
    test ("(−10)^2", "100", 0);
    test ("−(10^2)", "−100", 0);
    test ("2^100", "1267650600228229401496703205376", 0);
    test ("4^3^2", "262144", 0);
    test ("4^(3^2)", "262144", 0);
    test ("(4^3)^2", "4096", 0);
    test ("√4", "2", 0);
    test ("√4−2", "0", 0);
    test ("∛8", "2", 0);
    test ("∜16", "2", 0);
    test ("₃√8", "2", 0);
    test ("₁₀√1024", "2", 0);
    test ("√(2+2)", "2", 0);
    test ("2√4", "4", 0);
    test ("2×√4", "4", 0);
    test ("Sqrt (4)", "2", 0);
    test ("Sqrt (2)", "1.414213562", 0);
    test ("4^0.5", "2", 0);
    test ("2^0.5", "1.414213562", 0);
    test ("₃√−8", "−2", 0);
    test ("(−8)^(1÷3)", "−2", 0);

    test ("0 mod 7", "0", 0);
    test ("6 mod 7", "6", 0);
    test ("7 mod 7", "0", 0);
    test ("8 mod 7", "1", 0);
    test ("−1 mod 7", "6", 0);

    test ("21 mod 9", "3", 0);
    test ("21 mod -9", "−6", 0);
    test ("-21 mod 9", "6", 0);
    test ("-21 mod -9", "−3", 0);

    test ("sgn 0", "0", 0);  
    test ("sgn 3", "1", 0);
    test ("sgn −3", "−1", 0);
    test ("⌊3⌋", "3", 0);
    test ("⌈3⌉", "3", 0);
    test ("[3]", "3", 0);
    test ("⌊−3⌋", "−3", 0);
    test ("⌈−3⌉", "−3", 0);
    test ("[−3]", "−3", 0);
    test ("⌊3.2⌋", "3", 0);
    test ("⌈3.2⌉", "4", 0);
    test ("[3.2]", "3", 0);
    test ("⌊−3.2⌋", "−4", 0);
    test ("⌈−3.2⌉", "−3", 0);
    test ("[−3.2]", "−3", 0);
    test ("⌊3.5⌋", "3", 0);
    test ("⌈3.5⌉", "4", 0);
    test ("[3.5]", "4", 0);
    test ("⌊−3.5⌋", "−4", 0);
    test ("⌈−3.5⌉", "−3", 0);
    test ("[−3.5]", "−4", 0);
    test ("⌊3.7⌋", "3", 0);
    test ("⌈3.7⌉", "4", 0);
    test ("[3.7]", "4", 0);
    test ("⌊−3.7⌋", "−4", 0);
    test ("⌈−3.7⌉", "−3", 0);
    test ("[−3.7]", "−4", 0);
    test ("{3.2}", "0.2", 0);
    test ("{−3.2}", "0.8", 0);

    test ("|1|", "1", 0);
    test ("|−1|", "1", 0);
    test ("|3−5|", "2", 0);
    test ("|x|", "2", 0);
    test ("abs 1", "1", 0);
    test ("abs (−1)", "1", 0);

    test ("log 0", "", ErrorCode.MP);
    test ("log 1", "0", 0);
    test ("log 2", "0.301029996", 0);
    test ("log 10", "1", 0);
    test ("log₁₀ 10", "1", 0);
    test ("log₂ 2", "1", 0);
    test ("2 log 2", "0.602059991", 0);

    test ("ln 0", "", ErrorCode.MP);
    test ("ln 1", "0", 0);
    test ("ln 2", "0.693147181", 0);
    test ("ln e", "1", 0);
    test ("2 ln 2", "1.386294361", 0);

    angle_units = AngleUnit.DEGREES;
    test ("sin 0", "0", 0);
    test ("sin 45 − 1÷√2", "0", 0);
    test ("sin 20 + sin(−20)", "0", 0);
    test ("sin 90", "1", 0);
    test ("sin 180", "0", 0);
    test ("2 sin 90", "2", 0);
    test ("sin²45", "0.5", 0);

    test ("cos 0", "1", 0);
    test ("cos 45 − 1÷√2", "0", 0);
    test ("cos 20 − cos (−20)", "0", 0);
    test ("cos 90", "0", 0);
    test ("cos 180", "−1", 0);
    test ("2 cos 0", "2", 0);
    test ("cos²45", "0.5", 0);

    test ("tan 0", "0", 0);
    test ("tan 10 − sin 10÷cos 10", "0", 0);
    test ("tan 90", "", ErrorCode.MP);
    test ("tan 10", "0.176326981", 0);
    test ("tan²10", "0.031091204", 0);

    test ("cos⁻¹ 0", "90", 0);
    test ("cos⁻¹ 1", "0", 0);
    test ("cos⁻¹ (−1)", "180", 0);
    test ("cos⁻¹ (1÷√2)", "45", 0);
    test ("acos 0", "90", 0);
    test ("acos 1", "0", 0);

    test ("sin⁻¹ 0", "0", 0);
    test ("sin⁻¹ 1", "90", 0);
    test ("sin⁻¹ (−1)", "−90", 0);
    test ("sin⁻¹ (1÷√2)", "45", 0);
    test ("asin 0", "0", 0);
    test ("asin 1", "90", 0);

    test ("cosh 0", "1", 0);
    test ("cosh 10 − (e^10 + e^−10)÷2", "0", 0);

    test ("sinh 0", "0", 0);
    test ("sinh 10 − (e^10 − e^−10)÷2", "0", 0);
    test ("sinh (−10) + sinh 10", "0", 0);

    test ("cosh² (−5) − sinh² (−5)", "1", 0);
    test ("tanh 0", "0", 0);
    test ("tanh 10 − sinh 10 ÷ cosh 10", "0", 0);

    test ("atanh 0", "0", 0);
    test ("atanh (1÷10) − 0.5 ln(11÷9)", "0", 0);

    angle_units = AngleUnit.DEGREES;
    test ("sin 90", "1", 0);

    angle_units = AngleUnit.RADIANS;
    test ("sin (π÷2)", "1", 0); // FIXME: Shouldn't need brackets

    angle_units = AngleUnit.GRADIANS;
    test ("sin 100", "1", 0);

    /* Complex numbers */
    angle_units = AngleUnit.DEGREES;
    test ("i", "i", 0);
    test ("−i", "−i", 0);
    test ("2i", "2i", 0);
    test ("1+i", "1+i", 0);  
    test ("i+1", "1+i", 0);
    test ("1−i", "1−i", 0);  
    test ("i−1", "−1+i", 0);
    test ("i×i", "−1", 0);
    test ("i÷i", "1", 0);
    test ("1÷i", "−i", 0);
    test ("|i|", "1", 0);
    test ("|3+4i|", "5", 0);
    test ("arg 0", "", ErrorCode.MP);  
    test ("arg 1", "0", 0);
    test ("arg (1+i)", "45", 0);
    test ("arg i", "90", 0);
    test ("arg (−1+i)", "135", 0);
    test ("arg −1", "180", 0);
    test ("arg (1+−i)", "−45", 0);
    test ("arg −i", "−90", 0);
    test ("arg (−1−i)", "−135", 0);
    test ("i⁻¹", "−i", 0);
    test ("√−1", "i", 0);
    test ("(−1)^0.5", "i", 0);
    test ("√−4", "2i", 0);
    test ("e^iπ", "−1", 0);
    test ("log (−10) − (1 + πi÷ln(10))", "0", 0);
    test ("ln (−e) − (1 + πi)", "0", 0);
    test ("sin(iπ÷4) − i×sinh(π÷4)", "0", 0);
    test ("cos(iπ÷4) − cosh(π÷4)", "0", 0);

    /* Boolean */
    test ("0 and 0", "0", 0);
    test ("1 and 0", "0", 0);
    test ("0 and 1", "0", 0);
    test ("1 and 1", "1", 0);
    test ("3 and 5", "1", 0);

    test ("0 or 0", "0", 0);  
    test ("1 or 0", "1", 0);
    test ("0 or 1", "1", 0);  
    test ("1 or 1", "1", 0);
    test ("3 or 5", "7", 0);

    test ("0 xor 0", "0", 0);
    test ("1 xor 0", "1", 0);
    test ("0 xor 1", "1", 0);
    test ("1 xor 1", "0", 0);
    test ("3 xor 5", "6", 0);

    number_base = 16;
    test ("ones 1", "FFFFFFFE", 0);
    test ("ones 7FFFFFFF", "80000000", 0);
    test ("twos 1", "FFFFFFFF", 0);
    test ("twos 7FFFFFFF", "80000001", 0);
    test ("~7A₁₆", "FFFFFF85", 0);

    number_base = 2;
    wordlen = 4;
    test ("1100∧1010", "1000", 0);
    test ("1100∨1010", "1110", 0);
    test ("1100⊻1010", "110", 0);
    test ("1100⊕1010", "110", 0);
    //test ("1100⊼1010", "0111", 0);
    //test ("1100⊽1010", "0001", 0);
    //wordlen = 2;
    //test ("¬01₂", "10₂", 0);
    //test ("¬¬10₂", "10₂", 0);
}

private void test_base_conversion ()
{
    number_base = 10;
    wordlen = 32;
    angle_units = AngleUnit.DEGREES;
    enable_conversions = true;
    enable_variables = true;

    test ("10 in bin", "1010₂", 0);
    test ("10 in oct", "12₈", 0);
    test ("10 in dec", "10", 0);
    test ("10 in hex", "A₁₆", 0);
    test ("10 in binary", "1010₂", 0);
    test ("10 in octal", "12₈", 0);
    test ("10 in decimal", "10", 0);
    test ("10 in hexadecimal", "A₁₆", 0);

    test ("1010₂ in dec", "10", 0);
    test ("12₈ in dec", "10", 0);
    test ("10 in dec", "10", 0);
    test ("A₁₆ in dec", "10", 0);

    test ("x in bin", "10₂", 0);
    test ("x in oct", "2₈", 0);
    test ("x in dec", "2", 0);
    test ("x in hex", "2₁₆", 0);
}

private void test_precedence ()
{
    number_base = 10;
    wordlen = 32;
    angle_units = AngleUnit.DEGREES;
    enable_conversions = true;
    enable_variables = true;

    test ("1 + 2 - 3 * 4 / 5", "0.6", 0);
    test ("10 mod 4 / 2", "1", 0);
    test ("20 / 10 mod 3", "2", 0);
    test ("12 / 3 √4", "8", 0);
    test ("√5!", "10.95445115", 0);
    test ("4 ^ sin 30", "2", 0);
    test ("4 ^ (sin 30)", "2", 0);
    test ("4 ^ sin (30)", "2", 0);
    test ("sin (30) ^ 4", "0.0625", 0);
    test ("sin 30 ^ 4", "0.0625", 0);
    test ("sin (30 ^ 4)", "0", 0);

    test ("10 / - 2", "−5", 0);
    test ("10 * - 2", "−20", 0);
    test ("10 ^ -2", "0.01", 0);
    test ("-10 ^ 2", "−100", 0);
    test ("sin (-30)", "−0.5", 0);
    test ("sin - 30", "−0.5", 0);

    test ("6 + 3!", "12", 0);
    test ("4 * 3!", "24", 0);
    test ("100 mod 3!", "4", 0);
    test ("5! mod 7", "1", 0);
    test ("24 / 3!", "4", 0);
    test ("4! / 6", "4", 0);
    test ("cos 5!", "−0.5", 0);
    test ("sin 6!", "0", 0);
    test ("- 4!", "−24", 0);
    test ("3! ^ 3", "216", 0);
    test ("3 ^ 3!", "729", 0);
}

public int main (string[] args)
{
    Intl.setlocale (LocaleCategory.ALL, "C");

    test_conversions ();
    test_equations ();
    test_base_conversion ();
    test_precedence ();

    if (fail_count == 0)
        stdout.printf ("Passed all %i tests\n", pass_count);
    else
        stdout.printf ("Failed %i/%d tests\n", fail_count, pass_count + fail_count);

    return fail_count;
}
