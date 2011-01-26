#ifndef CURRENCY_H
#define CURRENCY_H

#include <glib/gi18n.h>

#include "mp.h"

typedef struct {
    char *short_name;
    char *symbol;
    char *long_name;
} CurrencyInfo;
static const CurrencyInfo currency_info[] = {
    {"AED", "إ.د",  N_("United Arab Emirates dirham")},
    {"AUD", "$",    N_("Australian dollar")},
    {"BGN", "лв",   N_("Bulgarian lev")},
    {"BHD", ".ب.د", N_("Bahraini dinar")},
    {"BND", "$",    N_("Brunei dollar")},
    {"BRL", "R$",   N_("Brazilian real")},
    {"BWP", "P",    N_("Botswana pula")},
    {"CAD", "$",    N_("Canadian dollar")},
    {"CHF", "Fr",   N_("Swiss franc")},
    {"CLP", "$",    N_("Chilean peso")},
    {"CNY", "元",   N_("Chinese yuan renminbi")},
    {"COP", "$",    N_("Colombian peso")},
    {"CZK", "Kč",   N_("Czech koruna")},
    {"DKK", "kr",   N_("Danish krone")},
    {"DZD", "ج.د",  N_("Algerian dinar")},
    {"EEK", "KR",   N_("Estonian kroon")},
    {"EUR", "€",    N_("Euro")},
    {"GBP", "£",    N_("Pound sterling")},
    {"HKD", "$",    N_("Hong Kong dollar")},
    {"HRK", "kn",   N_("Croatian kuna")},
    {"HUF", "Ft",   N_("Hungarian forint")},
    {"IDR", "Rp",   N_("Indonesian rupiah")},
    {"ILS", "₪",    N_("Israeli new shekel")},
    {"INR", "₹",    N_("Indian rupee")},
    {"IRR", "﷼",    N_("Iranian rial")},
    {"ISK", "kr",   N_("Icelandic krona")},
    {"JPY", "¥",    N_("Japanese yen")},
    {"KRW", "₩",    N_("South Korean won")},
    {"KWD", "ك.د",  N_("Kuwaiti dinar")},
    {"KZT", "₸",    N_("Kazakhstani tenge")},
    {"LKR", "Rs",   N_("Sri Lankan rupee")},
    {"LTL", "Lt",   N_("Lithuanian litas")},
    {"LVL", "Ls",   N_("Latvian lats")},
    {"LYD", "د.ل",  N_("Libyan dinar")},
    {"MUR", "Rs",   N_("Mauritian rupee")},
    {"MXN", "$",    N_("Mexican peso")},
    {"MYR", "RM",   N_("Malaysian ringgit")},
    {"NOK", "kr",   N_("Norwegian krone")},
    {"NPR", "Rs",   N_("Nepalese rupee")},
    {"NZD", "$",    N_("New Zealand dollar")},
    {"OMR", "ع.ر.", N_("Omani rial")},
    {"PEN", "S/.",  N_("Peruvian nuevo sol")},
    {"PHP", "₱",    N_("Philippine peso")},
    {"PKR", "Rs",   N_("Pakistani rupee")},
    {"PLN", "zł",   N_("Polish zloty")},
    {"QAR", "ق.ر",  N_("Qatari riyal")},
    {"RON", "L",    N_("New Romanian leu")},
    {"RUB", "руб.", N_("Russian rouble")},
    {"SAR", "س.ر",  N_("Saudi riyal")},
    {"SEK", "kr",   N_("Swedish krona")},
    {"SGD", "$",    N_("Singapore dollar")},
    {"THB", "฿",    N_("Thai baht")},
    {"TND", "ت.د",  N_("Tunisian dinar")},
    {"TRY", "TL",   N_("New Turkish lira")},
    {"TTD", "$",    N_("Trinidad and Tobago dollar")},
    {"USD", "$",    N_("US dollar")},
    {"UYU", "$",    N_("Uruguayan peso")},
    {"VEF", "Bs F", N_("Venezuelan bolívar")},
    {"ZAR", "R",    N_("South African rand")},
    {NULL, NULL}
};

// FIXME: Should indicate when rates are updated to UI

const CurrencyInfo *currency_get_info(const gchar *name);

MPNumber *currency_get_value(const gchar *currency);

#endif /* CURRENCY_H */
