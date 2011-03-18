/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2009 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef MP_INTERNAL_H
#define MP_INTERNAL_H

#include <glib/gi18n.h>

/* If we're not using GNU C, elide __attribute__ */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

#define min(a, b)   ((a) <= (b) ? (a) : (b))
#define max(a, b)   ((a) >= (b) ? (a) : (b))

//2E0 BELOW ENSURES AT LEAST ONE GUARD DIGIT
//MP.t = (int) ((float) (accuracy) * log((float)10.) / log((float) MP_BASE) + (float) 2.0);
//if (MP.t > MP_SIZE) {
//    mperr("MP_SIZE TOO SMALL IN CALL TO MPSET, INCREASE MP_SIZE AND DIMENSIONS OF MP ARRAYS TO AT LEAST %d ***", MP.t);
//    MP.t = MP_SIZE;
//}
#define MP_T 100

void mperr(const char *format, ...) __attribute__((format(printf, 1, 2)));
void mp_gcd(int64_t *, int64_t *);
void mp_normalize(MPNumber *);
void convert_to_radians(const MPNumber *x, MPAngleUnit unit, MPNumber *z);
void convert_from_radians(const MPNumber *x, MPAngleUnit unit, MPNumber *z);

#endif /* MP_INTERNAL_H */
