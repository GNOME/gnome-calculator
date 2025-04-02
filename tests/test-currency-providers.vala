/*
 * Copyright (C) 2021 Robert Roth.
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

private int fail_count = 0;
private int pass_count = 0;

private string error_code_to_string (ErrorCode error)
{
    if (error == ErrorCode.MP)
        return "ErrorCode.MP(\"%s\")".printf (Number.error);

    return mp_error_code_to_string (error);
}

private class TestConversion : Equation
{
    public TestConversion (string equation)
    {
        base (equation);
    }

    public override Number? convert (Number x, string x_units, string z_units,
                                     out Unit? x_unit, out Unit? z_unit)
    {
        return UnitManager.get_default ().convert_by_symbol (x, x_units, z_units,
                                                            out x_unit, out z_unit);
    }
}

private void test_currency (CurrencyManager currency_manager, string currency_str, string? expected, ErrorCode expected_error)
{
    var result_currency = currency_manager.get_currency (currency_str);

    uint representation_base = 10;

    if (result_currency == null)
    {
        if (expected == null)
        {
            pass_count++;
        }
        else
        {
            stdout.printf ("*FAIL: '%s' currency should not be available\n", currency_str);
            fail_count++;
        }
    }
    else
    {
        var serializer = new Serializer (DisplayFormat.FIXED, number_base, 9);
        serializer.set_representation_base (representation_base);
        var result = result_currency.get_value ();
        var result_str = serializer.to_string (result);

        if (expected_error != ErrorCode.NONE)
        {
            stdout.printf ("*FAIL: '%s' -> %s, expected error %s\n", currency_str, result_str, error_code_to_string (expected_error));
            fail_count++;
        }
        else if (result_str != expected)
        {
            stdout.printf ("*FAIL: '%s' -> '%s', expected '%s'\n", currency_str, result_str, expected);
            fail_count++;
        }
        else
        {
            /*stdout.printf ("PASS: '%s' -> '%s'\n", expression, result_str);*/
            pass_count++;
        }
    }
}

private void test (string expression, string expected, ErrorCode expected_error)
{
    var equation = new TestConversion (expression);
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


private void test_imf_provider ()
{
    var currency_manager = new CurrencyManager();
    var imf_provider = new OfflineImfCurrencyProvider (currency_manager, Path.build_filename (Environment.get_variable ("TESTS_ROOT"), "rms_five.xls"));

    imf_provider.clear ();
    test_currency (currency_manager, "EUR", null, 0);
    // set refresh interval and sync update
    imf_provider.refresh_interval = 3600;
    imf_provider.update_rates (false);

}

private void test_ecb_provider ()
{
}

private void test_currency_conversions ()
{
    var currency_manager = CurrencyManager.get_default (false, false);
    new OfflineImfCurrencyProvider (currency_manager, Path.build_filename (Environment.get_variable ("TESTS_ROOT"), "rms_five.xls"));
    currency_manager.refresh_interval = 3600;
    currency_manager.initialize_providers ();
    currency_manager.refresh_sync ();

    test ("1 EUR in EUR", "1", 0);
}

public int main (string[] args)
{
    Intl.setlocale (LocaleCategory.ALL, "C");

    test_imf_provider ();
    test_ecb_provider ();
    test_currency_conversions ();

    if (fail_count == 0)
        stdout.printf ("Passed all %i tests\n", pass_count);
    else
        stdout.printf ("Failed %i/%d tests\n", fail_count, pass_count + fail_count);

    return fail_count;
}
