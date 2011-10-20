/*
 * Copyright (C) 2008-2011 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#include <time.h>

#include <glib.h>
#include <glib/gstdio.h>
#include <gio/gio.h>
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
#include <glib/gi18n.h>

#include "currency-manager.h"
#include "mp.h"

typedef struct {
    char *short_name;
    char *symbol;
    char *long_name;
} CurrencyInfo;
static const CurrencyInfo currency_info[] = {
    {"AED", "إ.د",  N_("UAE Dirham")},
    {"AUD", "$",    N_("Australian Dollar")},
    {"BGN", "лв",   N_("Bulgarian Lev")},
    {"BHD", ".ب.د", N_("Bahraini Dinar")},
    {"BND", "$",    N_("Brunei Dollar")},
    {"BRL", "R$",   N_("Brazilian Real")},
    {"BWP", "P",    N_("Botswana Pula")},
    {"CAD", "$",    N_("Canadian Dollar")},
    {"CFA", "Fr",   N_("CFA Franc")},
    {"CHF", "Fr",   N_("Swiss Franc")},
    {"CLP", "$",    N_("Chilean Peso")},
    {"CNY", "元",   N_("Chinese Yuan")},
    {"COP", "$",    N_("Colombian Peso")},
    {"CZK", "Kč",   N_("Czech Koruna")},
    {"DKK", "kr",   N_("Danish Krone")},
    {"DZD", "ج.د",  N_("Algerian Dinar")},
    {"EEK", "KR",   N_("Estonian Kroon")},
    {"EUR", "€",    N_("Euro")},
    {"GBP", "£",    N_("Pound Sterling")},
    {"HKD", "$",    N_("Hong Kong Dollar")},
    {"HRK", "kn",   N_("Croatian Kuna")},
    {"HUF", "Ft",   N_("Hungarian Forint")},
    {"IDR", "Rp",   N_("Indonesian Rupiah")},
    {"ILS", "₪",    N_("Israeli New Shekel")},
    {"INR", "₹",    N_("Indian Rupee")},
    {"IRR", "﷼",    N_("Iranian Rial")},
    {"ISK", "kr",   N_("Icelandic Krona")},
    {"JPY", "¥",    N_("Japanese Yen")},
    {"KRW", "₩",    N_("South Korean Won")},
    {"KWD", "ك.د",  N_("Kuwaiti Dinar")},
    {"KZT", "₸",    N_("Kazakhstani Tenge")},
    {"LKR", "Rs",   N_("Sri Lankan Rupee")},
    {"LTL", "Lt",   N_("Lithuanian Litas")},
    {"LVL", "Ls",   N_("Latvian Lats")},
    {"LYD", "د.ل",  N_("Libyan Dinar")},
    {"MUR", "Rs",   N_("Mauritian Rupee")},
    {"MXN", "$",    N_("Mexican Peso")},
    {"MYR", "RM",   N_("Malaysian Ringgit")},
    {"NOK", "kr",   N_("Norwegian Krone")},
    {"NPR", "Rs",   N_("Nepalese Rupee")},
    {"NZD", "$",    N_("New Zealand Dollar")},
    {"OMR", "ع.ر.", N_("Omani Rial")},
    {"PEN", "S/.",  N_("Peruvian Nuevo Sol")},
    {"PHP", "₱",    N_("Philippine Peso")},
    {"PKR", "Rs",   N_("Pakistani Rupee")},
    {"PLN", "zł",   N_("Polish Zloty")},
    {"QAR", "ق.ر",  N_("Qatari Riyal")},
    {"RON", "L",    N_("New Romanian Leu")},
    {"RUB", "руб.", N_("Russian Rouble")},
    {"SAR", "س.ر",  N_("Saudi Riyal")},
    {"SEK", "kr",   N_("Swedish Krona")},
    {"SGD", "$",    N_("Singapore Dollar")},
    {"THB", "฿",    N_("Thai Baht")},
    {"TND", "ت.د",  N_("Tunisian Dinar")},
    {"TRY", "TL",   N_("New Turkish Lira")},
    {"TTD", "$",    N_("T&T Dollar (TTD)")},
    {"USD", "$",    N_("US Dollar")},
    {"UYU", "$",    N_("Uruguayan Peso")},
    {"VEF", "Bs F", N_("Venezuelan Bolívar")},
    {"ZAR", "R",    N_("South African Rand")},
    {NULL, NULL}
};

static gboolean downloading_imf_rates = FALSE, downloading_ecb_rates = FALSE;
static gboolean loaded_rates = FALSE;
static gboolean load_rates(CurrencyManager *manager);

struct CurrencyManagerPrivate
{
    GList *currencies;
};

G_DEFINE_TYPE (CurrencyManager, currency_manager, G_TYPE_OBJECT);


enum {
    UPDATED,
    LAST_SIGNAL
};
static guint signals[LAST_SIGNAL] = { 0, };

static CurrencyManager *default_currency_manager = NULL;


CurrencyManager *
currency_manager_get_default(void)
{
    int i;

    if (default_currency_manager)
        return default_currency_manager;

    default_currency_manager = g_object_new(currency_manager_get_type(), NULL);

    for (i = 0; currency_info[i].short_name; i++) {
        Currency *c = currency_new(currency_info[i].short_name,
                                   _(currency_info[i].long_name),
                                   currency_info[i].symbol);
        default_currency_manager->priv->currencies = g_list_append(default_currency_manager->priv->currencies, c);
    }

    return default_currency_manager;
}


GList *
currency_manager_get_currencies(CurrencyManager *manager)
{
    g_return_val_if_fail(manager != NULL, NULL);
    return manager->priv->currencies;
}


Currency *
currency_manager_get_currency(CurrencyManager *manager, const gchar *name)
{
    g_return_val_if_fail(manager != NULL, NULL);
    g_return_val_if_fail(name != NULL, NULL);

    GList *link;
    for (link = manager->priv->currencies; link; link = link->next) {
        Currency *c = link->data;
        const MPNumber *value;

        value = currency_get_value(c);

        if (!strcmp(name, currency_get_name(c))) {
            if (mp_is_negative(value) ||
                mp_is_zero(value)) {
                return NULL;
            }
            else
                return c;
        }
    }
    return NULL;
}


static char *
get_imf_rate_filepath()
{
    return g_build_filename(g_get_user_cache_dir (),
                            "gcalctool",
                            "rms_five.xls",
                            NULL);
}


static char *
get_ecb_rate_filepath()
{
    return g_build_filename(g_get_user_cache_dir (),
                            "gcalctool",
                            "eurofxref-daily.xml",
                            NULL);
}


static Currency *
add_currency(CurrencyManager *manager, const gchar *short_name)
{
    GList *iter;
    Currency *c;

    for (iter = manager->priv->currencies; iter; iter = iter->next) {
        c = iter->data;
        if (strcmp(short_name, currency_get_name(c)) == 0)
            return c;
    }

    g_warning("Currency %s is not in the currency table", short_name);
    c = currency_new(short_name, short_name, short_name);
    manager->priv->currencies = g_list_append(manager->priv->currencies, c);

    return c;
}


/* A file needs to be redownloaded if it doesn't exist, or is too old.
 * When an error occur, it probably won't hurt to try to download again.
 */
static gboolean
file_needs_update(gchar *filename, double max_age)
{
    struct stat buf;

    if (!g_file_test(filename, G_FILE_TEST_IS_REGULAR))
        return TRUE;

    if (g_stat(filename, &buf) == -1)
        return TRUE;

    if (difftime(time(NULL), buf.st_mtime) > max_age)
        return TRUE;

    return FALSE;
}


static void
download_imf_cb(GObject *object, GAsyncResult *result, gpointer user_data)
{
    CurrencyManager *manager = user_data;
    GError *error = NULL;

    if (g_file_copy_finish(G_FILE(object), result, &error))
        g_debug("IMF rates updated");
    else
        g_warning("Couldn't download IMF currency rate file: %s", error->message);
    g_clear_error(&error);
    downloading_imf_rates = FALSE;
    load_rates(manager);
}


static void
download_ecb_cb(GObject *object, GAsyncResult *result, gpointer user_data)
{
    CurrencyManager *manager = user_data;
    GError *error = NULL;

    if (g_file_copy_finish(G_FILE(object), result, &error))
        g_debug("ECB rates updated");
    else
        g_warning("Couldn't download ECB currency rate file: %s", error->message);
    g_clear_error(&error);
    downloading_ecb_rates = FALSE;
    load_rates(manager);
}


static void
download_file(CurrencyManager *manager, gchar *uri, gchar *filename, GAsyncReadyCallback callback)
{
    gchar *directory;
    GFile *source, *dest;

    directory = g_path_get_dirname(filename);
    g_mkdir_with_parents(directory, 0755);
    g_free(directory);

    source = g_file_new_for_uri(uri);
    dest = g_file_new_for_path(filename);

    g_file_copy_async(source, dest, G_FILE_COPY_OVERWRITE, G_PRIORITY_DEFAULT, NULL, NULL, NULL, callback, manager);
    g_object_unref(source);
    g_object_unref(dest); 
}


static void
load_imf_rates(CurrencyManager *manager)
{
    gchar *filename;
    gchar *data, **lines;
    gsize length;
    GError *error = NULL;
    int i;
    gboolean result, in_data = FALSE;
    struct 
    {
        const gchar *name, *symbol;
    } name_map[] = 
    {
        {"Euro",                "EUR"},
        {"Japanese Yen",        "JPY"},
        {"U.K. Pound Sterling", "GBP"},
        {"U.S. Dollar",         "USD"},
        {"Algerian Dinar",      "DZD"},
        {"Australian Dollar",   "AUD"},
        {"Bahrain Dinar",       "BHD"},
        {"Botswana Pula",       "BWP"},
        {"Brazilian Real",      "BRL"},
        {"Brunei Dollar",       "BND"},
        {"Canadian Dollar",     "CAD"},
        {"Chilean Peso",        "CLP"},
        {"Chinese Yuan",        "CNY"},
        {"Colombian Peso",      "COP"},
        {"Czech Koruna",        "CZK"},
        {"Danish Krone",        "DKK"},
        {"Hungarian Forint",    "HUF"},
        {"Icelandic Krona",     "ISK"},
        {"Indian Rupee",        "INR"},
        {"Indonesian Rupiah",   "IDR"},
        {"Iranian Rial",        "IRR"},
        {"Israeli New Sheqel",  "ILS"},
        {"Kazakhstani Tenge",   "KZT"},
        {"Korean Won",          "KRW"},
        {"Kuwaiti Dinar",       "KWD"},
        {"Libyan Dinar",        "LYD"},
        {"Malaysian Ringgit",   "MYR"},
        {"Mauritian Rupee",     "MUR"},
        {"Mexican Peso",        "MXN"},
        {"Nepalese Rupee",      "NPR"},
        {"New Zealand Dollar",  "NZD"},
        {"Norwegian Krone",     "NOK"},
        {"Rial Omani",          "OMR"},
        {"Pakistani Rupee",     "PKR"},
        {"Nuevo Sol",           "PEN"},
        {"Philippine Peso",     "PHP"},
        {"Polish Zloty",        "PLN"},
        {"Qatar Riyal",         "QAR"},
        {"Russian Ruble",       "RUB"},
        {"Saudi Arabian Riyal", "SAR"},
        {"Singapore Dollar",    "SGD"},
        {"South African Rand",  "ZAR"},
        {"Sri Lanka Rupee",     "LKR"},
        {"Swedish Krona",       "SEK"},
        {"Swiss Franc",         "CHF"},
        {"Thai Baht",           "THB"},
        {"Trinidad And Tobago Dollar", "TTD"},
        {"Tunisian Dinar",      "TND"},
        {"U.A.E. Dirham",       "AED"},
        {"Peso Uruguayo",       "UYU"},
        {"Bolivar Fuerte",      "VEF"},
        {NULL, NULL}
    };

    filename = get_imf_rate_filepath();
    result = g_file_get_contents(filename, &data, &length, &error);
    g_free(filename);
    if (!result)
    {
        g_warning("Failed to read exchange rates: %s", error->message);
        g_clear_error(&error);
        return;
    }

    lines = g_strsplit(data, "\n", 0);
    g_free(data);

    for (i = 0; lines[i]; i++) {
        gchar *line, **tokens;

        line = g_strchug(lines[i]);
      
        /* Start after first blank line, stop on next */
        if (line[0] == '\0') {
            if (!in_data) {
               in_data = TRUE;
               continue;
            }
            else
               break;
        }
        if (!in_data)
            continue;

        tokens = g_strsplit(line, "\t", 0);
        if (strcmp(tokens[0], "Currency") != 0) {
            gint value_index, name_index;

            for (value_index = 1; tokens[value_index]; value_index++) {
                gchar *value = g_strchug (tokens[value_index]);
                if (value[0] != '\0')
                    break;
            }
            if (tokens[value_index]) {
                for (name_index = 0; name_map[name_index].name; name_index++) {
                    if (strcmp(name_map[name_index].name, tokens[0]) == 0)
                        break;
                }
                if (name_map[name_index].name) {
                    Currency *c = currency_manager_get_currency(manager, name_map[name_index].symbol);
                    MPNumber value;

                    if (!c) {
                        g_debug ("Using IMF rate of %s for %s", tokens[value_index], name_map[name_index].symbol);
                        c = add_currency(manager, name_map[name_index].symbol);
                    }
                    mp_set_from_string(tokens[value_index], 10, &value);
                    mp_reciprocal(&value, &value);
                    currency_set_value(c, &value);
                }
                else
                    g_warning("Unknown currency '%s'", tokens[0]);
            }
        }
        g_strfreev(tokens);
    }
    g_strfreev(lines);
}


static void
set_ecb_rate(CurrencyManager *manager, xmlNodePtr node, Currency *eur_rate)
{
    xmlAttrPtr attribute;
    gchar *name = NULL, *value = NULL;

    for (attribute = node->properties; attribute; attribute = attribute->next) {
        if (strcmp((char *)attribute->name, "currency") == 0) {
            if (name)
                xmlFree(name);
            name = (gchar *)xmlNodeGetContent((xmlNodePtr)attribute);
        } else if (strcmp ((char *)attribute->name, "rate") == 0) {
            if (value)
                xmlFree(value);
            value = (gchar *)xmlNodeGetContent((xmlNodePtr)attribute);
        }
    }

    /* Use data if value and no rate currently defined */
    if (name && value && !currency_manager_get_currency(manager, name)) {
        Currency *c;
        MPNumber r, v;

        g_debug ("Using ECB rate of %s for %s", value, name);
        c = add_currency(manager, name);
        mp_set_from_string(value, 10, &r);
        mp_set_from_mp(currency_get_value(eur_rate), &v);
        mp_multiply(&v, &r, &v);
        currency_set_value(c, &v);
    }

    if (name)
        xmlFree(name);
    if (value)
        xmlFree(value);
}


static void
set_ecb_fixed_rate(CurrencyManager *manager, const gchar *name, const gchar *value, Currency *eur_rate)
{
    Currency *c;
    MPNumber r, v;

    g_debug ("Using ECB fixed rate of %s for %s", value, name);
    c = add_currency(manager, name);
    mp_set_from_string(value, 10, &r);
    mp_set_from_mp(currency_get_value(eur_rate), &v);
    mp_divide(&v, &r, &v);
    currency_set_value(c, &v);
}


static void
load_ecb_rates(CurrencyManager *manager)
{
    Currency *eur_rate;
    char *filename;
    xmlDocPtr document;
    xmlXPathContextPtr xpath_ctx;
    xmlXPathObjectPtr xpath_obj;
    int i, len;

    /* Scale rates to the EUR value */
    eur_rate = currency_manager_get_currency(manager, "EUR");
    if (!eur_rate) {
        g_warning("Cannot use ECB rates as don't have EUR rate");
        return;
    }

    /* Set some fixed rates */
    set_ecb_fixed_rate(manager, "EEK", "0.06391", eur_rate);
    set_ecb_fixed_rate(manager, "CFA", "0.152449", eur_rate);

    xmlInitParser();
    filename = get_ecb_rate_filepath();
    document = xmlReadFile(filename, NULL, 0);
    if (!document)
        g_error("Couldn't parse ECB rate file %s", filename);
    g_free (filename);
    if (!document)    
        return;

    xpath_ctx = xmlXPathNewContext(document);
    if (xpath_ctx == NULL) {
        xmlFreeDoc(document);
        g_error("Couldn't create XPath context");
        return;
    }

    xmlXPathRegisterNs(xpath_ctx,
                       BAD_CAST("xref"),
                       BAD_CAST("http://www.ecb.int/vocabulary/2002-08-01/eurofxref"));
    xpath_obj = xmlXPathEvalExpression(BAD_CAST("//xref:Cube[@currency][@rate]"),
                                       xpath_ctx);
    if (xpath_obj == NULL) {
        xmlXPathFreeContext(xpath_ctx);
        xmlFreeDoc(document);
        fprintf(stderr, "Couldn't create XPath object\n");
        return;
    }
    len = (xpath_obj->nodesetval) ? xpath_obj->nodesetval->nodeNr : 0;
    for (i = 0; i < len; i++) {
        if (xpath_obj->nodesetval->nodeTab[i]->type == XML_ELEMENT_NODE)
            set_ecb_rate(manager, xpath_obj->nodesetval->nodeTab[i], eur_rate);

        /* Avoid accessing removed elements */
        if (xpath_obj->nodesetval->nodeTab[i]->type != XML_NAMESPACE_DECL)
            xpath_obj->nodesetval->nodeTab[i] = NULL;
    }

    xmlXPathFreeObject(xpath_obj);
    xmlXPathFreeContext(xpath_ctx);
    xmlFreeDoc(document);
    xmlCleanupParser();
}


static gboolean
load_rates(CurrencyManager *manager)
{
    int i;

    /* Already loaded */
    if (loaded_rates)
        return TRUE;

    /* In process */
    if (downloading_imf_rates || downloading_ecb_rates)
        return FALSE;

    /* Use the IMF provided values and top up with currencies tracked by the ECB and not the IMF */
    load_imf_rates(manager);
    load_ecb_rates(manager);

    for (i = 0; currency_info[i].short_name; i++) {
       GList *link;
       for (link = manager->priv->currencies; link; link = link->next) {
           Currency *c = link->data;
           if (strcmp(currency_get_name(c), currency_info[i].short_name) == 0)
              break;
       }
       if (!link)
           g_warning("Currency %s is not provided by IMF or ECB", currency_info[i].short_name);
    }

    g_debug("Rates loaded");
    loaded_rates = TRUE;

    g_signal_emit(manager, signals[UPDATED], 0);
  
    return TRUE;
}


const MPNumber *
currency_manager_get_value(CurrencyManager *manager, const gchar *currency)
{
    gchar *path;
    Currency *c;

    g_return_val_if_fail(manager != NULL, NULL);
    g_return_val_if_fail(currency != NULL, NULL);

    /* Update rates if necessary */
    path = get_imf_rate_filepath();
    if (!downloading_imf_rates && file_needs_update(path, 60 * 60 * 24 * 7)) {
        downloading_imf_rates = TRUE;
        g_debug("Downloading rates from the IMF...");
        download_file(manager, "http://www.imf.org/external/np/fin/data/rms_five.aspx?tsvflag=Y", path, download_imf_cb);
    }
    g_free(path);
    path = get_ecb_rate_filepath();
    if (!downloading_ecb_rates && file_needs_update(path, 60 * 60 * 24 * 7)) {
        downloading_ecb_rates = TRUE;
        g_debug("Downloading rates from the ECB...");
        download_file(manager, "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml", path, download_ecb_cb);
    }
    g_free(path);

    if (!load_rates(manager))
        return NULL;

    c = currency_manager_get_currency(manager, currency);
    if (c)
        return currency_get_value(c);
    else
        return NULL;
}


static void
currency_manager_class_init(CurrencyManagerClass *klass)
{
    g_type_class_add_private(klass, sizeof(CurrencyManagerPrivate));

    signals[UPDATED] =
        g_signal_new("updated",
                     G_TYPE_FROM_CLASS (klass),
                     G_SIGNAL_RUN_LAST,
                     G_STRUCT_OFFSET (CurrencyManagerClass, updated),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);
}


static void
currency_manager_init(CurrencyManager *manager)
{
    manager->priv = G_TYPE_INSTANCE_GET_PRIVATE(manager, currency_manager_get_type(), CurrencyManagerPrivate);
}
