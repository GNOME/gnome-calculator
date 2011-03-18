/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2011 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef FINANCIAL_H
#define FINANCIAL_H

#include "mp.h"
#include "math-equation.h"

void do_finc_expression(MathEquation *equation, int function, MPNumber *arg1, MPNumber *arg2, MPNumber *arg3, MPNumber *arg4);

enum finc_dialogs {
    FINC_CTRM_DIALOG,
    FINC_DDB_DIALOG,
    FINC_FV_DIALOG,
    FINC_GPM_DIALOG,
    FINC_PMT_DIALOG,
    FINC_PV_DIALOG,
    FINC_RATE_DIALOG,
    FINC_SLN_DIALOG,
    FINC_SYD_DIALOG,
    FINC_TERM_DIALOG
};

#endif /* FINANCIAL_H */
