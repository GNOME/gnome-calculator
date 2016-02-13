/*
 * Copyright (C) 2009 Rich Burridge
 * Copyright (C) 2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

private const int MAXLINE = 1024;

private static Serializer result_serializer;

static void solve (string equation)
{
    var tsep_string = Posix.nl_langinfo (Posix.NLItem.THOUSEP);
    if (tsep_string == null || tsep_string == "")
        tsep_string = " ";

    var decimal = Posix.nl_langinfo (Posix.NLItem.RADIXCHAR);
    if (decimal == null)
        decimal = "";

    var e = new Equation (equation.replace (tsep_string, "").replace (decimal, "."));
    e.base = 10;
    e.wordlen = 32;
    e.angle_units = AngleUnit.DEGREES;

    ErrorCode ret;
    uint representation_base;
    var z = e.parse (out representation_base, out ret);

    result_serializer.set_representation_base (representation_base);
    if (z != null)
    {
        var str = result_serializer.to_string (z);
        if (result_serializer.error != null)
        {
            stderr.printf ("%s\n", result_serializer.error);
            result_serializer.error = null;
        }
        else
            stdout.printf ("%s\n", str);
    }
    else if (ret == ErrorCode.MP)
        stderr.printf ("Error %s\n", Number.error);
    else
        stderr.printf ("Error %d\n", ret);
}

public static int main (string[] args)
{
    /* Seed random number generator. */
    var now = new DateTime.now_utc ();
    bool requires_new_line = false;

    Random.set_seed (now.get_microsecond ());

    Intl.setlocale (LocaleCategory.ALL, "");

    result_serializer = new Serializer (DisplayFormat.AUTOMATIC, 10, 9);

    var buffer = new char[1024];
    while (true)
    {
        stdout.printf ("> ");
        var line = stdin.gets (buffer);

        if (line != null)
            line = line.strip ();
        else
            requires_new_line = true;

        if (line == null || line == "exit" || line == "quit" || line == "")
            break;

        solve (line);
    }

    if (requires_new_line)
        stdout.printf ("\n");

    return Posix.EXIT_SUCCESS;
}
