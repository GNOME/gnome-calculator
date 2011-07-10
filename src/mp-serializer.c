/*
 * Copyright (C) 2010 Robin Sonefors
 * Copyright (C) 2008-2011 Robert Ancell.
 * 
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#include <langinfo.h>
#include <glib.h>
#include <glib-object.h>
#include <string.h>
#include <stdio.h>

#include "mp-serializer.h"
#include "mp-enums.h"

enum {
    PROP_0,
    PROP_SHOW_THOUSANDS_SEPARATORS,
    PROP_SHOW_TRAILING_ZEROES,
    PROP_NUMBER_FORMAT,
    PROP_BASE,
};

struct MpSerializerPrivate
{
    gint leading_digits;      /* Number of digits to show before radix */
    gint trailing_digits;     /* Number of digits to show after radix */
    MpDisplayFormat format;   /* Number display mode. */
    gint show_tsep;           /* Set if the thousands separator should be shown. */
    gint show_zeroes;         /* Set if trailing zeroes should be shown. */

    gint base;                /* Numeric base */

    gunichar tsep;            /* Locale specific thousands separator. */
    gunichar radix;           /* Locale specific radix string. */
    gint tsep_count;          /* Number of digits between separator. */
};


G_DEFINE_TYPE(MpSerializer, mp_serializer, G_TYPE_OBJECT);

MpSerializer *
mp_serializer_new(MpDisplayFormat format, int base, int trailing_digits)
{
    MpSerializer *serializer = g_object_new(mp_serializer_get_type(), /*"number-format", format,*/ NULL);
    mp_serializer_set_number_format(serializer, format);
    mp_serializer_set_base(serializer, base);
    mp_serializer_set_trailing_digits(serializer, trailing_digits);
    return serializer;
}


static void
mp_cast_to_string_real(MpSerializer *serializer, const MPNumber *x, int base, gboolean force_sign, int *n_digits, GString *string)
{
    static gchar digits[] = "0123456789ABCDEF";
    MPNumber number, integer_component, fractional_component, temp;
    int i, last_non_zero;

    if (mp_is_negative(x))
        mp_abs(x, &number);
    else
        mp_set_from_mp(x, &number);

    /* Add rounding factor */
    mp_set_from_integer(base, &temp);
    mp_xpowy_integer(&temp, -(serializer->priv->trailing_digits+1), &temp);
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

        if (serializer->priv->base == 10 && serializer->priv->show_tsep && i == serializer->priv->tsep_count) {
            g_string_prepend_unichar(string, serializer->priv->tsep);
            i = 0;
        }
        i++;

        mp_divide_integer(&temp, base, &t);
        mp_floor(&t, &t);
        mp_multiply_integer(&t, base, &t2);

        mp_subtract(&temp, &t2, &t3);

        d = mp_cast_to_int(&t3);
        g_string_prepend_c(string, d < 16 ? digits[d] : '?');
        (*n_digits)++;

        mp_set_from_mp(&t, &temp);
    } while (!mp_is_zero(&temp));

    last_non_zero = string->len;

    g_string_append_unichar(string, serializer->priv->radix);

    /* Write out the fractional component */
    mp_set_from_mp(&fractional_component, &temp);
    for (i = serializer->priv->trailing_digits; i > 0 && !mp_is_zero(&temp); i--) {
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
    if (!serializer->priv->show_zeroes || serializer->priv->trailing_digits == 0)
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
        const gchar *digits[] = {"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉"};
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


static gchar *
mp_cast_to_string(MpSerializer *serializer, const MPNumber *x, int *n_digits)
{
    GString *string;
    MPNumber x_real;
    gchar *result;

    string = g_string_sized_new(1024);

    mp_real_component(x, &x_real);
    mp_cast_to_string_real(serializer, &x_real, serializer->priv->base, FALSE, n_digits, string);
    if (mp_is_complex(x)) {
        GString *s;
        gboolean force_sign = TRUE;
        MPNumber x_im;
        int n_complex_digits;

        mp_imaginary_component(x, &x_im);

        if (strcmp(string->str, "0") == 0) {
            g_string_assign(string, "");
            force_sign = FALSE;
        }

        s = g_string_sized_new(1024);
        mp_cast_to_string_real(serializer, &x_im, 10, force_sign, &n_complex_digits, s);
        if (n_complex_digits > *n_digits)
            *n_digits = n_complex_digits;
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

    result = g_strndup(string->str, string->len + 1);
    g_string_free(string, TRUE);

    return result;
}


static int
mp_cast_to_exponential_string_real(MpSerializer *serializer, const MPNumber *x, GString *string, gboolean eng_format, int *n_digits)
{
    gchar *fixed;
    MPNumber t, z, base, base3, base10, base10inv, mantissa;
    int exponent = 0;

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

    fixed = mp_cast_to_string(serializer, &mantissa, n_digits);
    g_string_append(string, fixed);
    g_free(fixed);
  
    return exponent;
}


static void
append_exponent(GString *string, int exponent)
{
    const gchar *super_digits[] = {"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹"};
    gchar *super_value, *c;
  
    if (exponent == 0)
        return;

    g_string_append_printf(string, "×10"); // FIXME: Use the current base
    if (exponent < 0) {
        exponent = -exponent;
        g_string_append(string, "⁻");
    }

    super_value = g_strdup_printf("%d", exponent);
    for (c = super_value; *c; c++)
        g_string_append(string, super_digits[*c - '0']);
    g_free (super_value);
}


static gchar *
mp_cast_to_exponential_string(MpSerializer *serializer, const MPNumber *x, gboolean eng_format, int *n_digits)
{
    GString *string;
    MPNumber x_real;
    gchar *result;
    int exponent;

    string = g_string_sized_new(1024);

    mp_real_component(x, &x_real);
    exponent = mp_cast_to_exponential_string_real(serializer, &x_real, string, eng_format, n_digits);
    append_exponent(string, exponent);

    if (mp_is_complex(x)) {
        GString *s;
        MPNumber x_im;
        int n_complex_digits = 0;

        mp_imaginary_component(x, &x_im);

        if (strcmp(string->str, "0") == 0)
            g_string_assign(string, "");

        s = g_string_sized_new(1024);
        exponent = mp_cast_to_exponential_string_real(serializer, &x_im, s, eng_format, &n_complex_digits);
        if (n_complex_digits > *n_digits)
            *n_digits = n_complex_digits;
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
        append_exponent(string, exponent);
    }

    result = g_strndup(string->str, string->len + 1);
    g_string_free(string, TRUE);

    return result;
}


gchar *
mp_serializer_to_string(MpSerializer *serializer, const MPNumber *x)
{
    gchar *s0;
    int n_digits = 0;

    switch(serializer->priv->format) {
    default:
    case MP_DISPLAY_FORMAT_AUTOMATIC:
        s0 = mp_cast_to_string(serializer, x, &n_digits);
        if (n_digits <= serializer->priv->leading_digits)
            return s0;
        else {
            g_free (s0);
            return mp_cast_to_exponential_string(serializer, x, FALSE, &n_digits);
        }
        break;
    case MP_DISPLAY_FORMAT_FIXED:
        return mp_cast_to_string(serializer, x, &n_digits);
    case MP_DISPLAY_FORMAT_SCIENTIFIC:
        return mp_cast_to_exponential_string(serializer, x, FALSE, &n_digits);
    case MP_DISPLAY_FORMAT_ENGINEERING:
        return mp_cast_to_exponential_string(serializer, x, TRUE, &n_digits);
    }
}


gboolean
mp_serializer_from_string(MpSerializer *serializer, const gchar *str, MPNumber *z)
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


void
mp_serializer_set_radix(MpSerializer *serializer, gunichar radix)
{
    serializer->priv->radix = radix;
}


gunichar
mp_serializer_get_radix(MpSerializer *serializer)
{
    return serializer->priv->radix;
}


void
mp_serializer_set_thousands_separator(MpSerializer *serializer, gunichar separator)
{
    serializer->priv->tsep = separator;
}


gunichar
mp_serializer_get_thousands_separator(MpSerializer *serializer)
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
mp_serializer_get_leading_digits(MpSerializer *serializer)
{
    return serializer->priv->leading_digits;
}


void
mp_serializer_set_leading_digits(MpSerializer *serializer, int leading_digits)
{
    serializer->priv->leading_digits = leading_digits;
}


int
mp_serializer_get_trailing_digits(MpSerializer *serializer)
{
    return serializer->priv->trailing_digits;
}


void
mp_serializer_set_trailing_digits(MpSerializer *serializer, int trailing_digits)
{
    serializer->priv->trailing_digits = trailing_digits;
}


MpDisplayFormat
mp_serializer_get_number_format(MpSerializer *serializer)
{
    return serializer->priv->format;
}


void
mp_serializer_set_number_format(MpSerializer *serializer, MpDisplayFormat format)
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
                                    PROP_NUMBER_FORMAT,
                                    g_param_spec_enum("number-format",
                                                      "number-format",
                                                      "Display format",
                                                      math_mp_display_format_get_type(),
                                                      MP_DISPLAY_FORMAT_AUTOMATIC,
                                                      G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_BASE,
                                    g_param_spec_int("base",
                                                     "base",
                                                     "Default number base",
                                                     2, 16, 10, 
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
    serializer->priv->leading_digits = 12;
    serializer->priv->trailing_digits = 9;
    serializer->priv->show_zeroes = FALSE;
    serializer->priv->show_tsep = FALSE;
    serializer->priv->format = MP_DISPLAY_FORMAT_AUTOMATIC;
}
