/*  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 *  Copyright (c) 2008-2009 Robert Ancell
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 *  02111-1307, USA.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <glib-object.h>

#include "currency.h"
#include "unittest.h"
#include "get.h"
#include "math-window.h"
#include "register.h"
#include "mp-equation.h"


static MathEquation *equation;
static MathWindow *window;

static void
version(const gchar *progname)
{
    /* NOTE: Is not translated so can be easily parsed */
    fprintf(stderr, "%1$s %2$s\n", progname, VERSION);
}


static void
solve(const char *equation)
{
    MPEquationOptions options;
    MPErrorCode error;
    MPNumber result;
    char result_str[1024];

    memset(&options, 0, sizeof(options));
    options.base = 10;
    options.wordlen = 32;
    options.angle_units = MP_DEGREES;

    error = mp_equation_parse(equation, &options, &result, NULL);
    if(error == PARSER_ERR_MP) {
        fprintf(stderr, "Error: %s\n", mp_get_error());
        exit(1);
    }
    else if(error != 0) {
        fprintf(stderr, "Error: %s\n", mp_error_code_to_string(error));
        exit(1);
    }
    else {
        mp_cast_to_string(&result, 10, 10, 9, 1, result_str, 1024);
        printf("%s\n", result_str);
        exit(0);
    }
}


static void
usage(const gchar *progname, int show_gtk)
{
    fprintf(stderr,
            /* Description on how to use gcalctool displayed on command-line */
            _("Usage:\n"
              "  %s — Perform mathematical calculations"), progname);

    fprintf(stderr,
            "\n\n");

    fprintf(stderr,
            /* Description on gcalctool command-line help options displayed on command-line */
            _("Help Options:\n"
              "  -v, --version                   Show release version\n"
              "  -h, -?, --help                  Show help options\n"
              "  --help-all                      Show all help options\n"
              "  --help-gtk                      Show GTK+ options"));
    fprintf(stderr,
            "\n\n");

    if (show_gtk) {
        fprintf(stderr,
                /* Description on gcalctool command-line GTK+ options displayed on command-line */
                _("GTK+ Options:\n"
                  "  --class=CLASS                   Program class as used by the window manager\n"
                  "  --name=NAME                     Program name as used by the window manager\n"
                  "  --screen=SCREEN                 X screen to use\n"
                  "  --sync                          Make X calls synchronous\n"
                  "  --gtk-module=MODULES            Load additional GTK+ modules\n"
                  "  --g-fatal-warnings              Make all warnings fatal"));
        fprintf(stderr,
                "\n\n");
    }

    fprintf(stderr,
            /* Description on gcalctool application options displayed on command-line */
            _("Application Options:\n"
              "  -u, --unittest                  Perform unit tests\n"
              "  -s, --solve <equation>          Solve the given equation"));
    fprintf(stderr,
            "\n\n");
}


void
get_options(int argc, char *argv[])
{
    int i;
    char *progname, *arg;

    progname = g_path_get_basename(argv[0]);

    for (i = 1; i < argc; i++) {
        arg = argv[i];

        if (strcmp(arg, "-v") == 0 ||
            strcmp(arg, "--version") == 0) {
            version(progname);
            exit(0);
        }
        else if (strcmp(arg, "-h") == 0 ||
                 strcmp(arg, "-?") == 0 ||
                 strcmp(arg, "--help") == 0) {
            usage(progname, FALSE);
            exit(0);
        }
        else if (strcmp(arg, "--help-all") == 0) {
            usage(progname, TRUE);
            exit(0);
        }
        else if (strcmp(arg, "-s") == 0 ||
            strcmp(arg, "--solve") == 0) {
            i++;
            if (i >= argc) {
                fprintf(stderr,
                        /* Error printed to stderr when user uses --solve argument without an equation */
                        _("Argument --solve requires an equation to solve"));
                fprintf(stderr, "\n");
                exit(1);
            }
            else
                solve(argv[i]);
        }
        else if (strcmp(arg, "-u") == 0 ||
            strcmp(arg, "--unittest") == 0) {
            unittest();
        }
        else {
            fprintf(stderr,
                    /* Error printed to stderr when user provides an unknown command-line argument */
                    _("Unknown argument '%s'"), arg);
            fprintf(stderr, "\n");
            usage(progname, FALSE);
            exit(1);
        }
    }
}


static void
quit_cb(MathWindow *window)
{
    set_int_resource(R_ACCURACY, math_equation_get_accuracy(equation));
    set_int_resource(R_WORDLEN, math_equation_get_word_size(equation));
    set_boolean_resource(R_TSEP, math_equation_get_show_thousands_separators(equation));
    set_boolean_resource(R_ZEROES, math_equation_get_show_trailing_zeroes(equation));
    //FIXME
    //set_resource(R_DISPLAY, "?");
    //set_resource(R_TRIG, "?");
    //set_resource(R_MODE, "?");

    currency_free_resources();
    gtk_main_quit();
}


int
main(int argc, char **argv)
{
    int accuracy = 9, base = 10, word_size = 64;
    gchar *angle_units;
    gboolean show_tsep = FALSE, show_zeroes = FALSE;
    gchar *number_format, *angle_unit, *button_mode;
  
    g_type_init();

    bindtextdomain(GETTEXT_PACKAGE, LOCALE_DIR);
    bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
    textdomain(GETTEXT_PACKAGE);

    /* Seed random number generator. */
    srand48((long) time((time_t *) 0));

    resources_init();
    register_init();
    get_options(argc, argv);
  
    equation = math_equation_new();
    get_int_resource(R_ACCURACY, &accuracy);
    get_int_resource(R_WORDLEN, &word_size);
    get_boolean_resource(R_TSEP, &show_tsep);  
    get_boolean_resource(R_ZEROES, &show_zeroes);
    number_format = get_resource(R_DISPLAY);
    angle_units = get_resource(R_TRIG);
    button_mode = get_resource(R_MODE);

    math_equation_set_accuracy(equation, accuracy);
    math_equation_set_word_size(equation, word_size);
    math_equation_set_show_thousands_separators(equation, show_tsep);
    math_equation_set_show_trailing_zeroes(equation, show_zeroes);
    //FIXME
    //math_equation_set_number_format(equation, ?);
    //math_equation_set_angle_units(equation, ?);

    g_free(number_format);
    g_free(angle_units);
    g_free(button_mode);

    gtk_init(&argc, &argv);

    window = math_window_new(equation);
    g_signal_connect(G_OBJECT(window), "quit", G_CALLBACK(quit_cb), NULL);
    //FIXMEmath_buttons_set_mode(math_window_get_buttons(window), ADVANCED); // FIXME: We load the basic buttons even if we immediately switch to the next type

    gtk_widget_show(GTK_WIDGET(window));
    gtk_main();

    return(0);
}
