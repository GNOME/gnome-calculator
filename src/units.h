#ifndef UNITS_H
#define UNITS_H

#include "mp.h"

gboolean units_convert(const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z);

#endif /* UNITS_H */
