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
#include <gconf/gconf-client.h>

#include "currency.h"
#include "unittest.h"
#include "math-window.h"
#include "register.h"
#include "mp-equation.h"

static GConfClient *client = NULL;

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
usage(const gchar *progname, gboolean show_application, gboolean show_gtk)
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

    if (show_application) {
        fprintf(stderr,
                /* Description on gcalctool application options displayed on command-line */
                _("Application Options:\n"
                  "  -u, --unittest                  Perform unit tests\n"
                  "  -s, --solve <equation>          Solve the given equation"));
        fprintf(stderr,
                "\n\n");
    }
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
            usage(progname, TRUE, FALSE);
            exit(0);
        }
        else if (strcmp(arg, "--help-all") == 0) {
            usage(progname, TRUE, TRUE);
            exit(0);
        }
        else if (strcmp(arg, "--help-gtk") == 0) {
            usage(progname, FALSE, TRUE);
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
            usage(progname, TRUE, FALSE);
            exit(1);
        }
    }
}


// FIXME: Use a proper GEnum for these
static const gchar *
number_format_to_string(DisplayFormat format)
{
    switch(format)
    {
    default:
    case DEC:
        return "DEC";
    case BIN:
        return "BIN";
    case OCT:
        return "OCT";
    case HEX:
        return "HEX";
    case SCI:
        return "SCI";
    case ENG:      
        return "ENG";
    }
}


static DisplayFormat
string_to_number_format(const gchar *name)
{
    if (name == NULL)
        return DEC;

    if (strcmp(name, "BIN") == 0)
        return BIN;
    else if (strcmp(name, "OCT") == 0)
        return OCT;
    else if (strcmp(name, "HEX") == 0)
        return HEX;
    else if (strcmp(name, "SCI") == 0)
        return SCI;
    else if (strcmp(name, "ENG") == 0)
        return ENG;
    else
        return DEC;
}


static const gchar *
angle_unit_to_string(MPAngleUnit unit)
{
    switch(unit)
    {
    default:
    case MP_DEGREES:
        return "degrees";
    case MP_RADIANS:
        return "radians";
    case MP_GRADIANS:
        return "gradians";
    }
}


static MPAngleUnit
string_to_angle_unit(const gchar *name)
{
    if (name == NULL)
        return MP_DEGREES;

    if (strcmp(name, "radians") == 0)
        return MP_RADIANS;
    else if (strcmp(name, "gradians") == 0)
        return MP_GRADIANS;
    else
        return MP_DEGREES;
}


static const gchar *
button_mode_to_string(ButtonMode mode)
{
    switch(mode)
    {
    default:
    case BASIC:
        return "BASIC";
    case ADVANCED:
        return "ADVANCED";
    case FINANCIAL:
        return "FINANCIAL";
    case PROGRAMMING:
        return "PROGRAMMING";
    }
}


static ButtonMode
string_to_button_mode(const gchar *name)
{
    if (name == NULL)
         return BASIC;

    if (strcmp(name, "ADVANCED") == 0)
        return ADVANCED;
    else if (strcmp(name, "FINANCIAL") == 0)
        return FINANCIAL;
    else if (strcmp(name, "PROGRAMMING") == 0)
        return PROGRAMMING;
    else
        return BASIC;
}


static void
quit_cb(MathWindow *window)
{
    MathEquation *equation;
    MathButtons *buttons;

    equation = math_window_get_equation(window);
    buttons = math_window_get_buttons(window);

    gconf_client_set_int(client, "/apps/gcalctool/accuracy", math_equation_get_accuracy(equation), NULL);
    gconf_client_set_int(client, "/apps/gcalctool/wordlen", math_equation_get_word_size(equation), NULL);
    gconf_client_set_bool(client, "/apps/gcalctool/showthousands", math_equation_get_show_thousands_separators(equation), NULL);
    gconf_client_set_bool(client, "/apps/gcalctool/showzeroes", math_equation_get_show_trailing_zeroes(equation), NULL);
    gconf_client_set_string(client, "/apps/gcalctool/result_format", number_format_to_string(math_equation_get_number_format(equation)), NULL);
    gconf_client_set_string(client, "/apps/gcalctool/angle_units", angle_unit_to_string(math_equation_get_angle_units(equation)), NULL);
    gconf_client_set_string(client, "/apps/gcalctool/button_layout", button_mode_to_string(math_buttons_get_mode(buttons)), NULL);

    currency_free_resources();
    gtk_main_quit();
}


static void
get_int(const char *name, gint *value)
{
    GConfValue *v;

    v = gconf_client_get(client, name, NULL);
    if (!v)
        return;
    *value = gconf_value_get_int(v);
    gconf_value_free(v);
}


static void
get_bool(const char *name, gboolean *value)
{
    GConfValue *v;

    v = gconf_client_get(client, name, NULL);
    if (!v)
        return;
    *value = gconf_value_get_bool(v);
    gconf_value_free(v);
}


int
main(int argc, char **argv)
{
    MathEquation *equation;
    int accuracy = 9, word_size = 64;
    gboolean show_tsep = FALSE, show_zeroes = FALSE;
    gchar *number_format, *angle_units, *button_mode;
  
    g_type_init();

    bindtextdomain(GETTEXT_PACKAGE, LOCALE_DIR);
    bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
    textdomain(GETTEXT_PACKAGE);

    /* Seed random number generator. */
    srand48((long) time((time_t *) 0));

    register_init();
    get_options(argc, argv);

    client = gconf_client_get_default();
    gconf_client_add_dir(client, "/apps/gcalctool", GCONF_CLIENT_PRELOAD_NONE, NULL);  
  
    equation = math_equation_new();
    get_int("/apps/gcalctool/accuracy", &accuracy);
    get_int("/apps/gcalctool/wordlen", &word_size);
    get_bool("/apps/gcalctool/showthousands", &show_tsep);
    get_bool("/apps/gcalctool/showzeroes", &show_zeroes);
    number_format = gconf_client_get_string(client, "/apps/gcalctool/result_format", NULL);
    angle_units = gconf_client_get_string(client, "/apps/gcalctool/angle_units", NULL);
    button_mode = gconf_client_get_string(client, "/apps/gcalctool/button_layout", NULL);

    math_equation_set_accuracy(equation, accuracy);
    math_equation_set_word_size(equation, word_size);
    math_equation_set_show_thousands_separators(equation, show_tsep);
    math_equation_set_show_trailing_zeroes(equation, show_zeroes);
    math_equation_set_number_format(equation, string_to_number_format(number_format));
    math_equation_set_angle_units(equation, string_to_angle_unit(angle_units));
    g_free(number_format);
    g_free(angle_units);

    gtk_init(&argc, &argv);

    window = math_window_new(equation);
    g_signal_connect(G_OBJECT(window), "quit", G_CALLBACK(quit_cb), NULL);
    math_buttons_set_mode(math_window_get_buttons(window), string_to_button_mode(button_mode)); // FIXME: We load the basic buttons even if we immediately switch to the next type
    g_free(button_mode);

    gtk_widget_show(GTK_WIDGET(window));
    gtk_main();

    return(0);
}
