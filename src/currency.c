#include <time.h>

#include <glib.h>
#include <glib/gstdio.h>
#include <gio/gio.h>
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>

#include "currency.h"
#include "mp.h"

typedef struct {
    char *short_name;
    MPNumber value;
    const CurrencyInfo *info;
} Currency;

static GList *currencies = NULL;

static gboolean downloading_imf_rates = FALSE, downloading_ecb_rates = FALSE;
static gboolean loaded_rates = FALSE;

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
add_currency(const gchar *short_name)
{
    Currency *c;
    int i;

    c = g_malloc0(sizeof(Currency));
    c->short_name = g_strdup(short_name);
    for (i = 0; currency_info[i].short_name; i++) {
        if (strcmp(c->short_name, currency_info[i].short_name) == 0) {
            c->info = &currency_info[i];
            break;
        }
    }
    if (!c->info)
        g_warning("Currency %s is not in the currency table", c->short_name);

    currencies = g_list_append(currencies, c);

    return c;
}


static Currency *
get_currency(const gchar *short_name)
{
    GList *link;
    for (link = currencies; link; link = link->next) {
        Currency *c = link->data;
        if (!strcmp(short_name, c->short_name)) {
            if (mp_is_negative(&c->value) ||
                mp_is_zero(&c->value)) {
                return NULL;
            }
            else
                return c;
        }
    }
    return NULL;
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
    GError *error = NULL;

    if (g_file_copy_finish(G_FILE(object), result, &error))
        g_debug("IMF rates updated");
    else
        g_warning("Couldn't download IMF currency rate file: %s", error->message);
    g_clear_error(&error);
    downloading_imf_rates = FALSE;
}


static void
download_ecb_cb(GObject *object, GAsyncResult *result, gpointer user_data)
{
    GError *error = NULL;

    if (g_file_copy_finish(G_FILE(object), result, &error))
        g_debug("ECB rates updated");
    else
        g_warning("Couldn't download ECB currency rate file: %s", error->message);
    g_clear_error(&error);
    downloading_ecb_rates = FALSE;
}


static void
download_file(gchar *uri, gchar *filename, GAsyncReadyCallback callback)
{
    gchar *directory;
    GFile *source, *dest;

    directory = g_path_get_dirname(filename);
    g_mkdir_with_parents(directory, 0755);
    g_free(directory);

    source = g_file_new_for_uri(uri);
    dest = g_file_new_for_path(filename);

    g_file_copy_async(source, dest, G_FILE_COPY_OVERWRITE, G_PRIORITY_DEFAULT, NULL, NULL, NULL, callback, NULL);
    g_object_unref(source);
    g_object_unref(dest); 
}


static void
load_imf_rates()
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
                    Currency *c = get_currency(name_map[name_index].symbol);
                    if (!c) {
                        g_debug ("Using IMF rate of %s for %s", tokens[value_index], name_map[name_index].symbol);
                        c = add_currency(name_map[name_index].symbol);
                    }
                    mp_set_from_string(tokens[value_index], 10, &c->value);
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
set_ecb_rate(xmlNodePtr node, Currency *eur_rate)
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
    if (name && value && !get_currency(name)) {
        Currency *c;
        MPNumber r;

        g_debug ("Using ECB rate of %s for %s", value, name);
        c = add_currency(name);
        mp_set_from_string(value, 10, &r);
        mp_set_from_mp(&eur_rate->value, &c->value);
        mp_divide(&c->value, &r, &c->value);
    }

    if (name)
        xmlFree(name);
    if (value)
        xmlFree(value);
}


static void
set_ecb_fixed_rate(const gchar *name, const gchar *value, Currency *eur_rate)
{
    Currency *c;
    MPNumber r;

    g_debug ("Using ECB fixed rate of %s for %s", value, name);
    c = add_currency(name);
    mp_set_from_string(value, 10, &r);
    mp_set_from_mp(&eur_rate->value, &c->value);
    mp_divide(&c->value, &r, &c->value);
}


static void
load_ecb_rates()
{
    Currency *eur_rate;
    char *filename = get_ecb_rate_filepath();
    xmlDocPtr document;
    xmlXPathContextPtr xpath_ctx;
    xmlXPathObjectPtr xpath_obj;
    int i, len;

    /* Scale rates to the EUR value */
    eur_rate = get_currency("EUR");
    if (!eur_rate) {
        g_warning("Cannot use ECB rates as don't have EUR rate");
        return;
    }

    /* Set some fixed rates */
    set_ecb_fixed_rate("EEK", "15.6466", eur_rate);

    xmlInitParser();
    document = xmlReadFile(filename, NULL, 0);
    g_free (filename);
    if (document == NULL) {
        g_error("Couldn't parse ECB rate file %s", filename);
        return;
    }

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
            set_ecb_rate(xpath_obj->nodesetval->nodeTab[i], eur_rate);

        /* Avoid accessing removed elements */
        if (xpath_obj->nodesetval->nodeTab[i]->type != XML_NAMESPACE_DECL)
            xpath_obj->nodesetval->nodeTab[i] = NULL;
    }

    xmlXPathFreeObject(xpath_obj);
    xmlXPathFreeContext(xpath_ctx);
    xmlFreeDoc(document);
    xmlCleanupParser();
}


static void
load_rates()
{
    int i;

    /* Use the IMF provided values and top up with currencies tracked by the ECB and not the IMF */
    load_imf_rates();
    load_ecb_rates();

    for (i = 0; currency_info[i].short_name; i++) {
       GList *link;
       for (link = currencies; link; link = link->next) {
           Currency *c = link->data;
           if (c->info == &currency_info[i])
              break;
       }
       if (!link)
           g_warning("Currency %s is not provided by IMF or ECB", currency_info[i].short_name);
    }

    g_debug("Rates loaded");
    loaded_rates = TRUE;  
}


const CurrencyInfo *
currency_get_info(const gchar *name)
{
    int i = 0;
    while (currency_info[i].short_name && strcmp(name, currency_info[i].short_name) != 0)
        i++;
    if (currency_info[i].short_name)
        return &currency_info[i];
    else
        return NULL;
}


gboolean
currency_convert(const MPNumber *from_amount,
                 const char *source_currency, const char *target_currency,
                 MPNumber *to_amount)
{
    gchar *path;
    Currency *from_info, *to_info;

    /* Update rates if necessary */
    path = get_imf_rate_filepath();
    if (!downloading_imf_rates && file_needs_update(path, 60 * 60 * 24 * 7)) {
        downloading_imf_rates = TRUE;
        g_debug("Downloading rates from the IMF...");
        download_file("http://www.imf.org/external/np/fin/data/rms_five.aspx?tsvflag=Y", path, download_imf_cb);
    }
    g_free(path);
    path = get_ecb_rate_filepath();
    if (!downloading_ecb_rates && file_needs_update(path, 60 * 60 * 24 * 7)) {
        downloading_ecb_rates = TRUE;
        g_debug("Downloading rates from the ECB...");
        download_file("http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml", path, download_ecb_cb);
    }
    g_free(path);

    if (downloading_imf_rates || downloading_ecb_rates)
        return FALSE;

    if (!loaded_rates)
        load_rates();
  
    from_info = get_currency(source_currency);
    to_info = get_currency(target_currency);
    if (!from_info || !to_info)
        return FALSE;

    if (mp_is_zero(&from_info->value) ||
        mp_is_zero(&to_info->value)) {
        mp_set_from_integer(0, to_amount);
        return FALSE;
    }

    mp_divide(from_amount, &to_info->value, to_amount);
    mp_multiply(to_amount, &from_info->value, to_amount);

    return TRUE;
}

void
currency_free_resources()
{
    GList *link;

    for (link = currencies; link; link = link->next) {
        Currency *c = link->data;
        g_free(c->short_name);
        g_free(c);
    }
    g_list_free(currencies);
    currencies = NULL;
}
