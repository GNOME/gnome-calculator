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

#ifndef MP_SERIALIZER_H
#define MP_SERIALIZER_H

#include <glib.h>
#include <glib-object.h>

#include "mp.h"

G_BEGIN_DECLS

#define MP_TYPE_SERIALIZER (mp_serializer_get_type())
#define MP_SERIALIZER(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), mp_serializer_get_type(), MpSerializer))

typedef struct MpSerializerPrivate MpSerializerPrivate;

typedef struct {
    GObject parent;
    MpSerializerPrivate *priv;
} MpSerializer;

typedef struct {
    GObjectClass parent;
} MpSerializerClass;

/* Number display mode. */
typedef enum {
    MP_DISPLAY_FORMAT_AUTOMATIC,
    MP_DISPLAY_FORMAT_FIXED,
    MP_DISPLAY_FORMAT_SCIENTIFIC,
    MP_DISPLAY_FORMAT_ENGINEERING
} MpDisplayFormat;

GType mp_serializer_get_type(void);

MpSerializer *mp_serializer_new(MpDisplayFormat format, int base, int trailing_digits);

gchar *mp_serializer_to_string(MpSerializer *serializer, const MPNumber *z);
gboolean mp_serializer_from_string(MpSerializer *serializer, const gchar *str, MPNumber *z);

void mp_serializer_set_number_format(MpSerializer *serializer, MpDisplayFormat format);
MpDisplayFormat mp_serializer_get_number_format(MpSerializer *serializer);

void mp_serializer_set_base(MpSerializer *serializer, int base);
int mp_serializer_get_base(MpSerializer *serializer);

void mp_serializer_set_leading_digits(MpSerializer *serializer, int leading_digits);
int mp_serializer_get_leading_digits(MpSerializer *serializer);

void mp_serializer_set_trailing_digits(MpSerializer *serializer, int trailing_digits);
int mp_serializer_get_trailing_digits(MpSerializer *serializer);

void mp_serializer_set_radix(MpSerializer *serializer, gunichar radix);
gunichar mp_serializer_get_radix(MpSerializer *serializer);

void mp_serializer_set_thousands_separator(MpSerializer *serializer, gunichar separator);
gunichar mp_serializer_get_thousands_separator(MpSerializer *serializer);

gint mp_serializer_get_thousands_separator_count(MpSerializer *serializer);

void mp_serializer_set_show_trailing_zeroes(MpSerializer *serializer, gboolean visible);
gboolean mp_serializer_get_show_trailing_zeroes(MpSerializer *serializer);

void mp_serializer_set_show_thousands_separators(MpSerializer *serializer, gboolean visible);
gboolean mp_serializer_get_show_thousands_separators(MpSerializer *serializer);

G_END_DECLS

#endif /* MP_SERIALIZER_H */
