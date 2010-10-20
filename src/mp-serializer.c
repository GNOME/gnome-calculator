/*  Copyright (c) 2010 Robin Sonefors
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

#include <langinfo.h>
#include <glib.h>
#include <glib-object.h>
#include <string.h>
#include <stdio.h>

#include "mp-serializer.h"

#include "math-enums.h"

enum {
    PROP_0,
    PROP_SHOW_THOUSANDS_SEPARATORS,
    PROP_SHOW_TRAILING_ZEROES,
    PROP_NUMBER_FORMAT,
    PROP_BASE,
};

static GType number_format_type;

struct MpSerializerPrivate
{
    gint accuracy;            /* Number of digits to show */
    DisplayFormat format;     /* Number display mode. */
    gint show_tsep;           /* Set if the thousands separator should be shown. */
    gint show_zeroes;         /* Set if trailing zeroes should be shown. */

    gint base;                /* Numeric base */

    gunichar tsep;            /* Locale specific thousands separator. */
    gunichar radix;           /* Locale specific radix string. */
    gint tsep_count;          /* Number of digits between separator. */
};


G_DEFINE_TYPE(MpSerializer, mp_serializer, G_TYPE_OBJECT);

MpSerializer *
mp_serializer_new()
{
    return g_object_new(mp_serializer_get_type(), NULL);
}


static void
mp_cast_to_string_real(MpSerializer *serializer, const MPNumber *x, int base, bool force_sign, GString *string)
{
    static char digits[] = "0123456789ABCDEF";
    MPNumber number, integer_component, fractional_component, temp;
    int i, last_non_zero;

    if (mp_is_negative(x))
        mp_abs(x, &number);
    else
        mp_set_from_mp(x, &number);

    /* Add rounding factor */
    mp_set_from_integer(base, &temp);
    mp_xpowy_integer(&temp, -(serializer->priv->accuracy+1), &temp);
    mp_multiply_integer(&temp, base, &temp);
    mp_divide_integer(&temp, 2, &temp);
    mp_add(&number, &temp, &temp);

    /* If trying to add rounding factor causes overflow, don't add it */
    if (!mp_get_error())
        mp_set_from_mp(&temp, &number);

    /* Split into integer and fractional component */
    mp_floor(&number, &integer_component);
    mp_fractional_component(&number, &fractional_component);

    /* Write out the integer component least significant digit to most */
    mp_set_from_mp(&integer_component, &temp);
    i = 0;
    do {
        MPNumber t, t2, t3;
        int64_t d;

        mp_divide_integer(&temp, base, &t);
        mp_floor(&t, &t);
        mp_multiply_integer(&t, base, &t2);

        mp_subtract(&temp, &t2, &t3);

        d = mp_cast_to_int(&t3);
        g_string_prepend_c(string, d < 16 ? digits[d] : '?');

        i++;
        if (serializer->priv->show_tsep && i == serializer->priv->tsep_count) {
            g_string_prepend_unichar(string, serializer->priv->tsep);
            i = 0;
        }

        mp_set_from_mp(&t, &temp);
    } while (!mp_is_zero(&temp));

    last_non_zero = string->len;

    g_string_append_unichar(string, serializer->priv->radix);

    /* Write out the fractional component */
    mp_set_from_mp(&fractional_component, &temp);
    for (i = serializer->priv->accuracy; i > 0 && !mp_is_zero(&temp); i--) {
        int d;
        MPNumber digit;

        mp_multiply_integer(&temp, base, &temp);
        mp_floor(&temp, &digit);
        d = mp_cast_to_int(&digit);

        g_string_append_c(string, digits[d]);

        if(d != 0)
            last_non_zero = string->len;
        mp_subtract(&temp, &digit, &temp);
    }

    /* Strip trailing zeroes */
    if (!serializer->priv->show_zeroes || serializer->priv->accuracy == 0)
        g_string_truncate(string, last_non_zero);

    /* Add sign on non-zero values */
    if (strcmp(string->str, "0") != 0 || force_sign) {
        if (mp_is_negative(x))
            g_string_prepend(string, "−");
        else if (force_sign)
            g_string_prepend(string, "+");
    }

    /* Append base suffix if not in default base */
    if (base != serializer->priv->base) {
        const char *digits[] = {"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉"};
        int multiplier = 1;
        int b = base;

        while (base / multiplier != 0)
            multiplier *= 10;
        while (multiplier != 1) {
            int d;
            multiplier /= 10;
            d = b / multiplier;
            g_string_append(string, digits[d]);
            b -= d * multiplier;
        }
    }
}


static void
mp_cast_to_string(MpSerializer *serializer, const MPNumber *x, char **buffer)
{
    GString *string;
    MPNumber x_real;

    string = g_string_sized_new(1024);

    mp_real_component(x, &x_real);
    mp_cast_to_string_real(serializer, &x_real, serializer->priv->base, false, string);
    if (mp_is_complex(x)) {
        GString *s;
        gboolean force_sign = true;
        MPNumber x_im;

        mp_imaginary_component(x, &x_im);

        if (strcmp(string->str, "0") == 0) {
            g_string_assign(string, "");
            force_sign = false;
        }

        s = g_string_sized_new(1024);
        mp_cast_to_string_real(serializer, &x_im, 10, force_sign, s);
        if (strcmp(s->str, "0") == 0 || strcmp(s->str, "+0") == 0 || strcmp(s->str, "−0") == 0) {
            /* Ignore */
        }
        else if (strcmp(s->str, "1") == 0) {
            g_string_append(string, "i");
        }
        else if (strcmp(s->str, "+1") == 0) {
            g_string_append(string, "+i");
        }
        else if (strcmp(s->str, "−1") == 0) {
            g_string_append(string, "−i");
        }
        else {
            if (strcmp(s->str, "+0") == 0)
                g_string_append(string, "+");
            else if (strcmp(s->str, "0") != 0)
                g_string_append(string, s->str);

            g_string_append(string, "i");
        }
        g_string_free(s, TRUE);
    }

    *buffer = g_strndup(string->str, string->len + 1);
    g_string_free(string, TRUE);
}


static void
mp_cast_to_exponential_string(MpSerializer *serializer, const MPNumber *x, bool eng_format, char **buffer)
{
    char *fixed, *c;
    MPNumber t, z, base, base3, base10, base10inv, mantissa;
    int exponent = 0;
    GString *string;
    const char *super_digits[] = {"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹"};

    string = g_string_sized_new(1024);

    mp_abs(x, &z);
    if (mp_is_negative(x))
        g_string_append(string, "−");
    mp_set_from_mp(&z, &mantissa);

    mp_set_from_integer(serializer->priv->base, &base);
    mp_xpowy_integer(&base, 3, &base3);
    mp_xpowy_integer(&base, 10, &base10);
    mp_set_from_integer(1, &t);
    mp_divide(&t, &base10, &base10inv);

    if (!mp_is_zero(&mantissa)) {
        while (!eng_format && mp_is_greater_equal(&mantissa, &base10)) {
            exponent += 10;
            mp_multiply(&mantissa, &base10inv, &mantissa);
        }

        while ((!eng_format &&  mp_is_greater_equal(&mantissa, &base)) ||
                (eng_format && (mp_is_greater_equal(&mantissa, &base3) || exponent % 3 != 0))) {
            exponent += 1;
            mp_divide(&mantissa, &base, &mantissa);
        }

        while (!eng_format && mp_is_less_than(&mantissa, &base10inv)) {
            exponent -= 10;
            mp_multiply(&mantissa, &base10, &mantissa);
        }

        mp_set_from_integer(1, &t);
        while (mp_is_less_than(&mantissa, &t) || (eng_format && exponent % 3 != 0)) {
            exponent -= 1;
            mp_multiply(&mantissa, &base, &mantissa);
        }
    }

    mp_cast_to_string(serializer, &mantissa, &fixed);
    g_string_append(string, fixed);
    g_free(fixed);
    if (exponent != 0) {
        g_string_append_printf(string, "×10"); // FIXME: Use the current base
        if (exponent < 0) {
            exponent = -exponent;
            g_string_append(string, "⁻");
        }
        snprintf(fixed, 1024, "%d", exponent);
        for (c = fixed; *c; c++)
            g_string_append(string, super_digits[*c - '0']);
    }

    *buffer = g_strndup(string->str, string->len + 1);
    g_string_free(string, TRUE);
}


void
mp_serializer_to_standard_string(MpSerializer *serializer, const MPNumber *x, char **target)
{
    switch(serializer->priv->format) {
    case FIX:
        mp_cast_to_string(serializer, x, target);
        break;
    case SCI:
        mp_cast_to_exponential_string(serializer, x, false, target);
        break;
    case ENG:
        mp_cast_to_exponential_string(serializer, x, true, target);
        break;
    }
}


void
mp_serializer_to_specific_string(const MPNumber *x, int base, int accuracy, bool trim_zeroes, bool localize, char **target)
{
    MpSerializer *serializer = mp_serializer_new();
    if (!localize) {
        serializer->priv->radix = '.';
        serializer->priv->show_tsep = false;
    }
    serializer->priv->base = base;
    serializer->priv->accuracy = accuracy;
    serializer->priv->show_zeroes = !trim_zeroes;
    mp_serializer_to_standard_string(serializer, x, target);
    g_object_unref(serializer);
}


bool
mp_serializer_from_string(MpSerializer *serializer, const char *str, MPNumber *z)
{
    return mp_set_from_string(str, serializer->priv->base, z);
}


void
mp_serializer_set_base(MpSerializer *serializer, gint base)
{
    serializer->priv->base = base;
}


int
mp_serializer_get_base(MpSerializer *serializer)
{
    return serializer->priv->base;
}


gunichar
mp_serializer_get_numeric_point_text(MpSerializer *serializer)
{
    return serializer->priv->radix;
}


gunichar
mp_serializer_get_thousands_separator_text(MpSerializer *serializer)
{
    return serializer->priv->tsep;
}


gint
mp_serializer_get_thousands_separator_count(MpSerializer *serializer)
{
    return serializer->priv->tsep_count;
}


void
mp_serializer_set_show_thousands_separators(MpSerializer *serializer, gboolean visible)
{
    serializer->priv->show_tsep = visible;
}


gboolean
mp_serializer_get_show_thousands_separators(MpSerializer *serializer)
{
    return serializer->priv->show_tsep;
}


void
mp_serializer_set_show_trailing_zeroes(MpSerializer *serializer, gboolean visible)
{
    serializer->priv->show_zeroes = visible;
}


gboolean
mp_serializer_get_show_trailing_zeroes(MpSerializer *serializer)
{
    return serializer->priv->show_zeroes;
}


int
mp_serializer_get_accuracy(MpSerializer *serializer)
{
    return serializer->priv->accuracy;
}


void
mp_serializer_set_accuracy(MpSerializer *serializer, int accuracy)
{
    serializer->priv->accuracy = accuracy;
}


DisplayFormat
mp_serializer_get_number_format(MpSerializer *serializer)
{
    return serializer->priv->format;
}


void
mp_serializer_set_number_format(MpSerializer *serializer, DisplayFormat format)
{
    serializer->priv->format = format;
}


static void
mp_serializer_set_property(GObject *object,
                           guint prop_id,
                           const GValue *value,
                           GParamSpec *pspec)
{
    MpSerializer *self = MP_SERIALIZER(object);

    switch (prop_id) {
    case PROP_SHOW_THOUSANDS_SEPARATORS:
        mp_serializer_set_show_thousands_separators(self, g_value_get_boolean(value));
        break;
    case PROP_SHOW_TRAILING_ZEROES:
        mp_serializer_set_show_trailing_zeroes(self, g_value_get_boolean(value));
        break;
    case PROP_BASE:
        mp_serializer_set_base(self, g_value_get_int(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}


static void
mp_serializer_get_property(GObject *object,
                           guint prop_id,
                           GValue *value,
                           GParamSpec *pspec)
{
    MpSerializer *self = MP_SERIALIZER(object);

    switch (prop_id) {
    case PROP_SHOW_THOUSANDS_SEPARATORS:
        g_value_set_boolean(value, mp_serializer_get_show_thousands_separators(self));
        break;
    case PROP_SHOW_TRAILING_ZEROES:
        g_value_set_boolean(value, mp_serializer_get_show_trailing_zeroes(self));
        break;
    case PROP_BASE:
        g_value_set_int(value, mp_serializer_get_base(self));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}


static void
mp_serializer_class_init(MpSerializerClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    object_class->get_property = mp_serializer_get_property;
    object_class->set_property = mp_serializer_set_property;

    g_type_class_add_private(klass, sizeof(MpSerializerPrivate));

    number_format_type = math_display_format_get_type();

    g_object_class_install_property(object_class,
                                    PROP_SHOW_THOUSANDS_SEPARATORS,
                                    g_param_spec_boolean("show-thousands-separators",
                                                         "show-thousands-separators",
                                                         "Show thousands separators",
                                                         TRUE,
                                                         G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_SHOW_TRAILING_ZEROES,
                                    g_param_spec_boolean("show-trailing-zeroes",
                                                         "show-trailing-zeroes",
                                                         "Show trailing zeroes",
                                                         FALSE,
                                                         G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_BASE,
                                    g_param_spec_int("base",
                                                     "base",
                                                     "Default number base (derived from number-format)",
                                                     2, 16, 10, 
                                                     G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_NUMBER_FORMAT,
                                    g_param_spec_enum("number-format",
                                                      "number-format",
                                                      "Display format",
                                                      number_format_type,
                                                      FIX,
                                                      G_PARAM_READWRITE));
}


static void
mp_serializer_init(MpSerializer *serializer)
{
    gchar *radix, *tsep;
    serializer->priv = G_TYPE_INSTANCE_GET_PRIVATE(serializer, mp_serializer_get_type(), MpSerializerPrivate);

    radix = nl_langinfo(RADIXCHAR);
    serializer->priv->radix = radix ? g_utf8_get_char(g_locale_to_utf8(radix, -1, NULL, NULL, NULL)) : '.';
    tsep = nl_langinfo(THOUSEP);
    if (tsep && tsep[0] != '\0')
        serializer->priv->tsep = g_utf8_get_char(g_locale_to_utf8(tsep, -1, NULL, NULL, NULL));
    else
        serializer->priv->tsep = ' ';
    serializer->priv->tsep_count = 3;

    serializer->priv->base = 10;
    serializer->priv->accuracy = 9;
    serializer->priv->show_zeroes = FALSE;
    serializer->priv->show_tsep = FALSE;
    serializer->priv->format = FIX;
}
