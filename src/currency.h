#include <glib/gi18n.h>

#include "mp.h"

struct currency_name {
    char *short_name;
    char *long_name;
};

/*
 * List taken from http://www.ecb.int/press/pr/date/2008/html/pr081205.en.html
 * with euro added.
 */
static const struct currency_name currency_names[] = {
    {"AUD", N_("Australian dollar")},
    {"BGN", N_("Bulgarian lev")},
    {"BRL", N_("Brazilian real")},
    {"CAD", N_("Canadian dollar")},
    {"CHF", N_("Swiss franc")},
    {"CNY", N_("Chinese yuan renminbi")},
    {"CZK", N_("Czech koruna")},
    {"DKK", N_("Danish krone")},
    {"EEK", N_("Estonian kroon")},
    {"EUR", N_("Euro")},
    {"GBP", N_("Pound sterling")},
    {"HKD", N_("Hong Kong dollar")},
    {"HRK", N_("Croatian kuna")},
    {"HUF", N_("Hungarian forint")},
    {"IDR", N_("Indonesian rupiah")},
    {"INR", N_("Indian rupee")},
    {"ISK", N_("Icelandic krona")},
    {"JPY", N_("Japanese yen")},
    {"KRW", N_("South Korean won")},
    {"LTL", N_("Lithuanian litas")},
    {"LVL", N_("Latvian lats")},
    {"MXN", N_("Mexican peso")},
    {"MYR", N_("Malaysian ringgit")},
    {"NOK", N_("Norwegian krone")},
    {"NZD", N_("New Zealand dollar")},
    {"PHP", N_("Philippine peso")},
    {"PLN", N_("Polish zloty")},
    {"RON", N_("New Romanian leu")},
    {"RUB", N_("Russian rouble")},
    {"SEK", N_("Swedish krona")},
    {"SGD", N_("Singapore dollar")},
    {"THB", N_("Thai baht")},
    {"TRY", N_("New Turkish lira")},
    {"USD", N_("US dollar")},
    {"ZAR", N_("South African rand")},
    {NULL, NULL}
};

/* Returns 1 if the user needs to update his/her rates, 0 otherwise */
int currency_rates_needs_update();

/* Dowloads new rates. Returns 1 on success, 0 otherwise */
int currency_download_rates();

/* Loads rates from disk. Should be called after having made sure the rates
 * are not outdated, but before anything else. */
void currency_load_rates();

/* Returns an internal index for each short name to be used with
 * currency_convert. A negative value means invalid. */
int currency_get_index(const char *short_name);

/* Converts an amount of money from one currency to another */
void currency_convert(const MPNumber *from_amount,
                      int from_index,
                      int to_index,
                      MPNumber *to_amount);

/* Frees up all allocated resources */
void currency_free_resources();
