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
  FIX,
  SCI,
  ENG
} DisplayFormat;

GType mp_serializer_get_type(void);
MpSerializer *mp_serializer_new(void);

void mp_serializer_to_standard_string(MpSerializer *serializer, const MPNumber *z, char **target);
void mp_serializer_to_specific_string(const MPNumber *z, int base, int accuracy, bool trim_zeroes, bool localize, char **target);
bool mp_serializer_from_string(MpSerializer *serializer, const char *str, MPNumber *z);

gunichar mp_serializer_get_thousands_separator_text(MpSerializer *serializer);
gunichar mp_serializer_get_numeric_point_text(MpSerializer *serializer);
void mp_serializer_set_base(MpSerializer *serializer, int base);
int mp_serializer_get_base(MpSerializer *serializer);
gboolean mp_serializer_get_show_trailing_zeroes(MpSerializer *serializer);
void mp_serializer_set_show_trailing_zeroes(MpSerializer *serializer, gboolean visible);
gboolean mp_serializer_get_show_thousands_separators(MpSerializer *serializer);
void mp_serializer_set_show_thousands_separators(MpSerializer *serializer, gboolean visible);
int mp_serializer_get_accuracy(MpSerializer *serializer);
void mp_serializer_set_accuracy(MpSerializer *serializer, int accuracy);
DisplayFormat mp_serializer_get_number_format(MpSerializer *serializer);
void mp_serializer_set_number_format(MpSerializer *serializer, DisplayFormat format);

G_END_DECLS

#endif /* MP_SERIALIZER_H */
