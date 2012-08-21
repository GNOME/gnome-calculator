/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2011 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <locale.h>
#include <glib/gi18n.h>

#include "math-window.h"
#include "math-preferences.h"
#include "mp-equation.h"
#include "unit-manager.h"

static GSettings *settings = NULL;

static MathWindow *window;
static MathPreferencesDialog *preferences_dialog;

static void
version(const gchar *progname)
{
    /* NOTE: Is not translated so can be easily parsed */
    fprintf(stderr, "%1$s %2$s\n", progname, VERSION);
}


static int
do_convert(const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z, void *data)
{
    return unit_manager_convert_by_symbol(unit_manager_get_default(), x, x_units, z_units, z);
}


static void
solve(const char *equation)
{
    MPEquationOptions options;
    MPErrorCode error;
    MPNumber result;
    char *result_str;

    memset(&options, 0, sizeof(options));
    options.base = 10;
    options.wordlen = 32;
    options.angle_units = MP_DEGREES;
    options.convert = do_convert;

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
        result_str = mp_serializer_to_string(mp_serializer_new(MP_DISPLAY_FORMAT_AUTOMATIC, 10, 9), &result);
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
                  "  -s, --solve <equation>          Solve the given equation"));
        fprintf(stderr,
                "\n\n");
    }
}


static void
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


static void
accuracy_cb(MathEquation *equation, GParamSpec *spec)
{
    g_settings_set_int(settings, "accuracy", math_equation_get_accuracy(equation));
}


static void
word_size_cb(MathEquation *equation, GParamSpec *spec)
{
    g_settings_set_int(settings, "word-size", math_equation_get_word_size(equation));
}


static void
show_thousands_separators_cb(MathEquation *equation, GParamSpec *spec)
{
    g_settings_set_boolean(settings, "show-thousands", math_equation_get_show_thousands_separators(equation));
}


static void
show_trailing_zeroes_cb(MathEquation *equation, GParamSpec *spec)
{
    g_settings_set_boolean(settings, "show-zeroes", math_equation_get_show_trailing_zeroes(equation));
}


static void
number_format_cb(MathEquation *equation, GParamSpec *spec)
{
    g_settings_set_enum(settings, "number-format", math_equation_get_number_format(equation));
}


static void
angle_unit_cb(MathEquation *equation, GParamSpec *spec)
{
    g_settings_set_enum(settings, "angle-units", math_equation_get_angle_units(equation));
}


static void
source_currency_cb(MathEquation *equation, GParamSpec *spec)
{
    g_settings_set_string(settings, "source-currency", math_equation_get_source_currency(equation));
}


static void
target_currency_cb(MathEquation *equation, GParamSpec *spec)
{
    g_settings_set_string(settings, "target-currency", math_equation_get_target_currency(equation));
}


static void
source_units_cb(MathEquation *equation, GParamSpec *spec)
{
    g_settings_set_string(settings, "source-units", math_equation_get_source_units(equation));
}


static void
target_units_cb(MathEquation *equation, GParamSpec *spec)
{
    g_settings_set_string(settings, "target-units", math_equation_get_target_units(equation));
}


static void
programming_base_cb(MathButtons *buttons, GParamSpec *spec)
{
    g_settings_set_int(settings, "base", math_buttons_get_programming_base(buttons));
}


static void
mode_cb(MathButtons *buttons, GParamSpec *spec, GApplication *app)
{
    const char *state;
    GAction *action;

    g_settings_set_enum(settings, "button-mode", math_buttons_get_mode(buttons));

    switch(math_buttons_get_mode(buttons))
    {
    default:
    case BASIC:
      state = "basic";
      //FIXME: Should it revert to decimal mode? math_equation_set_number_format(window->priv->equation, DEC);
      break;

    case ADVANCED:
      state = "advanced";
      break;

    case FINANCIAL:
      state = "financial";
      break;

    case PROGRAMMING:
      state = "programming";
      break;
    }

    action = g_action_map_lookup_action(G_ACTION_MAP(app), "mode");
    g_simple_action_set_state(G_SIMPLE_ACTION(action),
                              g_variant_new_string(state));
}


static void
copy_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    math_equation_copy(math_window_get_equation(window));
}


static void
paste_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    math_equation_paste(math_window_get_equation(window));
}


static void
undo_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    math_equation_undo(math_window_get_equation(window));
}


static void
redo_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    math_equation_redo(math_window_get_equation(window));
}


static void
mode_changed_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    const char *mode_str;
    int mode = BASIC;

    mode_str = g_variant_get_string(parameter, NULL);
    if (strcmp(mode_str, "basic") == 0)
        mode = BASIC;
    else if (strcmp(mode_str, "advanced") == 0)
        mode = ADVANCED;
    else if (strcmp(mode_str, "financial") == 0)
        mode = FINANCIAL;
    else if (strcmp(mode_str, "programming") == 0)
        mode = PROGRAMMING;
    math_buttons_set_mode(math_window_get_buttons(window), mode);
}


static void
show_preferences_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    if (!preferences_dialog) {
        preferences_dialog = math_preferences_dialog_new(math_window_get_equation(window));
        gtk_window_set_transient_for(GTK_WINDOW(preferences_dialog), GTK_WINDOW(window));
    }
    gtk_window_present(GTK_WINDOW(preferences_dialog));
}


static void
help_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    GdkScreen *screen;
    GError *error = NULL;

    screen = gtk_widget_get_screen(GTK_WIDGET(window));
    gtk_show_uri(screen, "help:gcalctool", gtk_get_current_event_time(), &error);

    if (error != NULL)
    {
        GtkWidget *d;
        /* Translators: Error message displayed when unable to launch help browser */
        const char *message = _("Unable to open help file");

        d = gtk_message_dialog_new(GTK_WINDOW (window),
                                   GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE,
                                   "%s", message);
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG (d),
                                                 "%s", error->message);
        g_signal_connect(d, "response", G_CALLBACK(gtk_widget_destroy), NULL);
        gtk_window_present(GTK_WINDOW(d));

        g_error_free(error);
    }
}


static void
about_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    const gchar *authors[] = {
        "Rich Burridge <rich.burridge@gmail.com>",
        "Robert Ancell <robert.ancell@gmail.com>",
        "Klaus Niederkrüger <kniederk@umpa.ens-lyon.fr>",
        "Robin Sonefors <ozamosi@flukkost.nu>",
        NULL
    };
    const gchar *documenters[] = {
        "Sun Microsystems",
        NULL
    };

    /* The translator credits. Please translate this with your name(s). */
    const gchar *translator_credits = _("translator-credits");

    /* The license this software is under (GPL2+) */
    char *license = _("Gcalctool is free software; you can redistribute it and/or modify\n"
          "it under the terms of the GNU General Public License as published by\n"
          "the Free Software Foundation; either version 2 of the License, or\n"
          "(at your option) any later version.\n"
          "\n"
          "Gcalctool is distributed in the hope that it will be useful,\n"
          "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
          "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
          "GNU General Public License for more details.\n"
          "\n"
          "You should have received a copy of the GNU General Public License\n"
          "along with Gcalctool; if not, write to the Free Software Foundation, Inc.,\n"
          "51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA");

    gtk_show_about_dialog(GTK_WINDOW(window),
                          "name",
                          /* Program name in the about dialog */
                          _("Gcalctool"),
                          "version", VERSION,
                          "copyright",
                          /* Copyright notice in the about dialog */
                          _("\xc2\xa9 1986–2010 The Gcalctool authors"),
                          "license", license,
                          "comments",
                          /* Short description in the about dialog */
                          _("Calculator with financial and scientific modes."),
                          "authors", authors,
                          "documenters", documenters,
                          "translator_credits", translator_credits,
                          "logo-icon-name", "accessories-calculator",
                          NULL);
}


static void
quit_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    gtk_widget_destroy(GTK_WIDGET(window));
}


static GActionEntry app_entries[] = {
        { "copy", copy_cb, NULL, NULL, NULL },
        { "paste", paste_cb, NULL, NULL, NULL },
        { "undo", undo_cb, NULL, NULL, NULL },
        { "redo", redo_cb, NULL, NULL, NULL },
        { "mode", mode_changed_cb, "s", "\"basic\"", NULL },
        { "preferences", show_preferences_cb, NULL, NULL, NULL },
        { "help", help_cb, NULL, NULL, NULL },
        { "about", about_cb, NULL, NULL, NULL },
        { "quit", quit_cb, NULL, NULL, NULL },
};


static void
startup_cb(GApplication *application)
{
    MathEquation *equation;
    MathButtons *buttons;
    int accuracy = 9, word_size = 64, base = 10;
    gboolean show_tsep = FALSE, show_zeroes = FALSE;
    MpDisplayFormat number_format;
    MPAngleUnit angle_units;
    ButtonMode button_mode;
    gchar *source_currency, *target_currency;
    gchar *source_units, *target_units;
    GMenu *menu, *section;

    settings = g_settings_new ("org.gnome.gcalctool");
    accuracy = g_settings_get_int(settings, "accuracy");
    word_size = g_settings_get_int(settings, "word-size");
    base = g_settings_get_int(settings, "base");
    show_tsep = g_settings_get_boolean(settings, "show-thousands");
    show_zeroes = g_settings_get_boolean(settings, "show-zeroes");
    number_format = g_settings_get_enum(settings, "number-format");
    angle_units = g_settings_get_enum(settings, "angle-units");
    button_mode = g_settings_get_enum(settings, "button-mode");
    source_currency = g_settings_get_string(settings, "source-currency");
    target_currency = g_settings_get_string(settings, "target-currency");
    source_units = g_settings_get_string(settings, "source-units");
    target_units = g_settings_get_string(settings, "target-units");

    equation = math_equation_new();
    math_equation_set_accuracy(equation, accuracy);
    math_equation_set_word_size(equation, word_size);
    math_equation_set_show_thousands_separators(equation, show_tsep);
    math_equation_set_show_trailing_zeroes(equation, show_zeroes);
    math_equation_set_number_format(equation, number_format);
    math_equation_set_angle_units(equation, angle_units);
    math_equation_set_source_currency(equation, source_currency);
    math_equation_set_target_currency(equation, target_currency);
    math_equation_set_source_units(equation, source_units);
    math_equation_set_target_units(equation, target_units);
    g_free(source_currency);
    g_free(target_currency);
    g_free(source_units);
    g_free(target_units);

    g_signal_connect(equation, "notify::accuracy", G_CALLBACK(accuracy_cb), NULL);
    g_signal_connect(equation, "notify::word-size", G_CALLBACK(word_size_cb), NULL);
    g_signal_connect(equation, "notify::show-thousands-separators", G_CALLBACK(show_thousands_separators_cb), NULL);
    g_signal_connect(equation, "notify::show-trailing-zeroes", G_CALLBACK(show_trailing_zeroes_cb), NULL);
    g_signal_connect(equation, "notify::number-format", G_CALLBACK(number_format_cb), NULL);
    g_signal_connect(equation, "notify::angle-units", G_CALLBACK(angle_unit_cb), NULL);
    g_signal_connect(equation, "notify::source-currency", G_CALLBACK(source_currency_cb), NULL);
    g_signal_connect(equation, "notify::target-currency", G_CALLBACK(target_currency_cb), NULL);
    g_signal_connect(equation, "notify::source-units", G_CALLBACK(source_units_cb), NULL);
    g_signal_connect(equation, "notify::target-units", G_CALLBACK(target_units_cb), NULL);

    g_action_map_add_action_entries(G_ACTION_MAP(application), app_entries, G_N_ELEMENTS(app_entries), NULL);

    window = math_window_new(GTK_APPLICATION(application), equation);
    buttons = math_window_get_buttons(window);
    math_buttons_set_programming_base(buttons, base);
    math_buttons_set_mode(buttons, button_mode); // FIXME: We load the basic buttons even if we immediately switch to the next type
    g_signal_connect(buttons, "notify::programming-base", G_CALLBACK(programming_base_cb), NULL);
    g_signal_connect(buttons, "notify::mode", G_CALLBACK(mode_cb), application);
    mode_cb (buttons, NULL, application);

    menu = g_menu_new();

    section = g_menu_new();
    g_menu_append(section, _("Basic"), "app.mode::basic");
    g_menu_append(section, _("Advanced"), "app.mode::advanced");
    g_menu_append(section, _("Financial"), "app.mode::financial");
    g_menu_append(section, _("Programming"), "app.mode::programming");
    g_menu_append_section(menu, _("Mode"), G_MENU_MODEL(section));

    section = g_menu_new();
    g_menu_append(section, _("Preferences"), "app.preferences");
    g_menu_append_section(menu, NULL, G_MENU_MODEL(section));

    section = g_menu_new();
    g_menu_append(section, _("About Calculator"), "app.about");
    g_menu_append(section, _("Help"), "app.help");
    g_menu_append(section, _("Quit"), "app.quit");
    g_menu_append_section(menu, NULL, G_MENU_MODEL(section));

    gtk_application_set_app_menu(GTK_APPLICATION(application), G_MENU_MODEL(menu));

    gtk_application_add_accelerator(GTK_APPLICATION(application), "<control>Q", "app.quit", NULL);
    gtk_application_add_accelerator(GTK_APPLICATION(application), "F1", "app.help", NULL);
    gtk_application_add_accelerator(GTK_APPLICATION(application), "<control>C", "app.copy", NULL);
    gtk_application_add_accelerator(GTK_APPLICATION(application), "<control>V", "app.paste", NULL);
    gtk_application_add_accelerator(GTK_APPLICATION(application), "<control>Z", "app.undo", NULL);
    gtk_application_add_accelerator(GTK_APPLICATION(application), "<control><shift>Z", "app.redo", NULL);
}


static void
activate_cb(GApplication *application)
{
    gtk_window_present(GTK_WINDOW(window));
}


int
main(int argc, char **argv)
{
    GtkApplication *app;
    int status;

    setlocale(LC_ALL, "");
    bindtextdomain(GETTEXT_PACKAGE, LOCALE_DIR);
    bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
    textdomain(GETTEXT_PACKAGE);

    /* Seed random number generator. */
    srand48((long) time((time_t *) 0));

    g_type_init();

    get_options(argc, argv);

    gtk_init(&argc, &argv);

    gtk_window_set_default_icon_name("accessories-calculator");

    app = gtk_application_new("org.gnome.gcalctool", G_APPLICATION_FLAGS_NONE);
    g_signal_connect(app, "startup", G_CALLBACK(startup_cb), NULL);
    g_signal_connect(app, "activate", G_CALLBACK(activate_cb), NULL);

    status = g_application_run(G_APPLICATION(app), argc, argv);

    return status;
}
