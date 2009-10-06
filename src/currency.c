#include <time.h>

#include <glib.h>
#include <glib/gstdio.h>
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
#include <libsoup/soup.h>

#include "currency.h"
#include "mp.h"

typedef struct {
    char *short_name;
    MPNumber value;
} currency;

static currency *currencies = NULL;
static int currency_count = 0;

static char*
get_rate_filepath()
{
    return g_build_filename(g_get_user_cache_dir (),
                            "gcalctool",
                            "eurofxref-daily.xml",
                            NULL);
}

int
currency_get_index(const char *short_name)
{
    int i;
    for (i = 0; i < currency_count; i++) {
        if (!strcmp(short_name, currencies[i].short_name)) {
            if (mp_is_negative(&currencies[i].value) ||
                mp_is_zero(&currencies[i].value)) {
                return -1;
            } else {
                return i;
            }
        }
    }
    return -1;
}

/* A file needs to be redownloaded if it doesn't exist, or every 7 days.
 * When an error occur, it probably won't hurt to try to download again.
 */
int
currency_rates_needs_update()
{
    gchar *filename = get_rate_filepath ();
    struct stat buf;
    if (!g_file_test(filename, G_FILE_TEST_IS_REGULAR)) {
        g_free(filename);
        return 1;
    }

    if (g_stat(filename, &buf) == -1) {
        g_free(filename);
        return 1;
    }
    g_free(filename);

    if (difftime(buf.st_mtime, time(NULL)) > (60 * 60 * 24 * 7)) {
        return 1;
    }

    return 0;
}

/* FIXME: This code is synchronous, and should thus be accessed from a thread */
int
currency_download_rates()
{
    gchar *filename, *directory;
    GError *e = NULL;
    SoupSession *session = soup_session_sync_new();
    SoupMessage *msg = soup_message_new("GET",
                                        "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml");
    soup_session_send_message(session, msg);
    if (msg->status_code != 200) {
        return 0;
    }
    filename = get_rate_filepath();
    directory = g_path_get_dirname(filename);
    g_mkdir_with_parents(directory, 0755);
    g_free(directory);

    g_file_set_contents(filename,
                        msg->response_body->data,
                        msg->response_body->length,
                        &e);
    if (e != NULL) {
        fprintf(stderr, "Couldn't download currency file: %s\n", e->message);
        return 0;
    }
    g_free(filename);
    g_object_unref(session);

    return 1;
}

static void
set_rate (xmlNodePtr node, currency *cur)
{
    xmlAttrPtr attribute;
    for (attribute = node->properties; attribute; attribute = attribute->next) {
        if (strcmp((char *)attribute->name, "currency") == 0) {
            cur->short_name = (char *)xmlNodeGetContent((xmlNodePtr) attribute);
        } else if (strcmp ((char *)attribute->name, "rate") == 0) {
            char *val = (char *)xmlNodeGetContent ((xmlNodePtr) attribute);
            mp_set_from_string(val, &(cur->value));
            xmlFree (val);
        }
    }
}

void currency_load_rates()
{
    char *filename = get_rate_filepath();
    xmlDocPtr document;
    xmlXPathContextPtr xpath_ctx;
    xmlXPathObjectPtr xpath_obj;
    int i, len;

    g_return_if_fail(g_file_test(filename, G_FILE_TEST_IS_REGULAR));

    xmlInitParser();
    document = xmlReadFile(filename, NULL, 0);
    g_free (filename);
    if (document == NULL) {
        fprintf(stderr, "Couldn't parse data file\n");
        return;
    }

    xpath_ctx = xmlXPathNewContext(document);
    if (xpath_ctx == NULL) {
        xmlFreeDoc(document);
        fprintf(stderr, "Couldn't create XPath context\n");
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
    currency_count = len + 1;
    currencies = g_slice_alloc0(sizeof(currency) * currency_count);
    for (i = 0; i < len; i++) {
        if (xpath_obj->nodesetval->nodeTab[i]->type == XML_ELEMENT_NODE) {
            set_rate(xpath_obj->nodesetval->nodeTab[i], &currencies[i]);
        }

        // Avoid accessing removed elements
        if (xpath_obj->nodesetval->nodeTab[i]->type != XML_NAMESPACE_DECL)
            xpath_obj->nodesetval->nodeTab[i] = NULL;
    }

    currencies[len].short_name = g_strdup("EUR");
    MPNumber foo;
    mp_set_from_integer(1, &foo);
    currencies[len].value = foo;

    xmlXPathFreeObject(xpath_obj);
    xmlXPathFreeContext(xpath_ctx);
    xmlFreeDoc(document);
    xmlCleanupParser();
}

void
currency_convert(const MPNumber *from_amount,
                 int from_index,
                 int to_index,
                 MPNumber *to_amount)
{
    if (mp_is_zero(&currencies[from_index].value) ||
        mp_is_zero(&currencies[to_index].value)) {
        mp_set_from_integer(0, to_amount);
        return;
    }

    mp_divide(from_amount, &currencies[from_index].value, to_amount);
    mp_multiply(to_amount, &currencies[to_index].value, to_amount);
}

void
currency_free_resources()
{
    int i;

    for (i = 0; i < currency_count; i++) {
        if (currencies[i].short_name != NULL)
            xmlFree(currencies[i].short_name);
    }
    g_slice_free1(currency_count * sizeof(currency), currencies);
    currencies = NULL;
    currency_count = 0;
}
