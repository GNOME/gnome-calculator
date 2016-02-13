/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public enum FinancialDialog
{
    CTRM_DIALOG,
    DDB_DIALOG,
    FV_DIALOG,
    GPM_DIALOG,
    PMT_DIALOG,
    PV_DIALOG,
    RATE_DIALOG,
    SLN_DIALOG,
    SYD_DIALOG,
    TERM_DIALOG
}

public void do_finc_expression (MathEquation equation, FinancialDialog function, Number arg1, Number arg2, Number arg3, Number arg4)
{
    Number result;
    switch (function)
    {
    case FinancialDialog.CTRM_DIALOG:
        result = calc_ctrm (equation, arg1, arg2, arg3);
        break;
    case FinancialDialog.DDB_DIALOG:
        result = calc_ddb (equation, arg1, arg2, arg3);
        break;
    case FinancialDialog.FV_DIALOG:
        result = calc_fv (equation, arg1, arg2, arg3);
        break;
    case FinancialDialog.GPM_DIALOG:
        result = calc_gpm (equation, arg1, arg2);
        break;
    case FinancialDialog.PMT_DIALOG:
        result = calc_pmt (equation, arg1, arg2, arg3);
        break;
    case FinancialDialog.PV_DIALOG:
        result = calc_pv (equation, arg1, arg2, arg3);
        break;
    case FinancialDialog.RATE_DIALOG:
        result = calc_rate (equation, arg1, arg2, arg3);
        break;
    case FinancialDialog.SLN_DIALOG:
        result = calc_sln (equation, arg1, arg2, arg3);
        break;
    case FinancialDialog.SYD_DIALOG:
        result = calc_syd (equation, arg1, arg2, arg3, arg4);
        break;
    case FinancialDialog.TERM_DIALOG:
        result = calc_term (equation, arg1, arg2, arg3);
        break;
    default:
        result = new Number.integer (0);
        break;
    }

    equation.set_number (result);
}

private Number calc_ctrm (MathEquation equation, Number pint, Number fv, Number pv)
{
    /*  Cterm - pint (periodic interest rate).
     *          fv  (future value).
     *          pv  (present value).
     *
     *          RESULT = log (fv / pv) / log (1 + pint)
     */
    var t1 = fv.divide (pv);
    var t2 = t1.ln ();
    var t3 = pint.add (new Number.integer (1));
    var t4 = t3.ln ();
    return t2.divide (t4);
}

private Number calc_ddb (MathEquation equation, Number cost, Number life, Number period)
{
    /*  Ddb   - cost    (amount paid for asset).
     *          life    (useful life of the asset).
     *          period  (time period for depreciation allowance).
     *
     *          bv = 0.0;
     *          for (i = 0; i < life; i++)
     *            {
     *              VAL = ((cost - bv) * 2) / life
     *              bv += VAL
     *            }
     *          RESULT = VAL
     *
     */

    var z = new Number.integer (0);

    var tbv = new Number.integer (0);
    var len = period.to_integer ();
    for (var i = 0; i < len; i++)
    {
        var t1 = cost.subtract (tbv);
        var t2 = t1.multiply_integer (2);
        z = t2.divide (life);
        t1 = tbv;
        tbv = t1.add (z); /* TODO: why result is tbv, for next loop? */
    }

    if (len >= 0)
        equation.status = _("Error: the number of periods must be positive");

    return z;
}

private Number calc_fv (MathEquation equation, Number pmt, Number pint, Number n)
{
    /*  Fv    - pmt (periodic payment).
     *          pint (periodic interest rate).
     *          n   (number of periods).
     *
     *          RESULT = pmt * (pow (1 + pint, n) - 1) / pint
     */

    var t1 = pint.add (new Number.integer (1));
    var t2 = t1.xpowy (n);
    var t3 = t2.add (new Number.integer (-1));
    var t4 = pmt.multiply (t3);
    return t4.divide (pint);
}

private Number calc_gpm (MathEquation equation, Number cost, Number margin)
{
    /*  Gpm   - cost (cost of sale).
     *          margin (gross profit margin.
     *
     *          RESULT = cost / (1 - margin)
     */

    var t1 = new Number.integer (1);
    var t2 = t1.subtract (margin);
    return cost.divide (t2);
}

private Number calc_pmt (MathEquation equation, Number prin, Number pint, Number n)
{
    /*  Pmt   - prin (principal).
     *          pint  (periodic interest rate).
     *          n    (term).
     *
     *          RESULT = prin * (pint / (1 - pow (pint + 1, -1 * n)))
     */

    var t1 = pint.add (new Number.integer (1));
    var t2 = n.multiply_integer (-1);
    var t3 = t1.xpowy (t2);
    var t4 = t3.multiply_integer (-1);
    t1 = t4.add (new Number.integer (1));
    t2 = pint.divide (t1);
    return prin.multiply (t2);
}

private Number calc_pv (MathEquation equation, Number pmt, Number pint, Number n)
{
    /*  Pv    - pmt (periodic payment).
     *          pint (periodic interest rate).
     *          n   (term).
     *
     *          RESULT = pmt * (1 - pow (1 + pint, -1 * n)) / pint
     */

    var t1 = pint.add (new Number.integer (1));
    var t2 = n.multiply_integer (-1);
    var t3 = t1.xpowy (t2);
    var t4 = t3.multiply_integer (-1);
    t1 = t4.add (new Number.integer (1));
    t2 = t1.divide (pint);
    return pmt.multiply (t2);
}

private Number calc_rate (MathEquation equation, Number fv, Number pv, Number n)
{
    /*  Rate  - fv (future value).
     *          pv (present value).
     *          n  (term).
     *
     *          RESULT = pow (fv / pv, 1 / n) - 1
     */

    var t1 = fv.divide (pv);
    var t2 = new Number.integer (1);
    var t3 = t2.divide (n);
    var t4 = t1.xpowy (t3);
    return t4.add (new Number.integer (-1));
}

private Number calc_sln (MathEquation equation, Number cost, Number salvage, Number life)
{
    /*  Sln   - cost    (cost of the asset).
     *          salvage (salvage value of the asset).
     *          life    (useful life of the asset).
     *
     *          RESULT = (cost - salvage) / life
     */

    var t1 = cost.subtract (salvage);
    return t1.divide (life);
}

private Number calc_syd (MathEquation equation, Number cost, Number salvage, Number life, Number period)
{
    /*  Syd   - cost    (cost of the asset).
     *          salvage (salvage value of the asset).
     *          life    (useful life of the asset).
     *          period  (period for which depreciation is computed).
     *
     *          RESULT = (cost - salvage) * (life - period + 1) /
     *                   (life * (life + 1)) / 2
     */

    var t3 = life.subtract (period).add (new Number.integer (1));
    var t2 = life.add (new Number.integer (1));
    var t4 = life.multiply (t2);
    var t1 = t4.divide (new Number.integer (2));
    t2 = t3.divide (t1);
    t1 = cost.subtract (salvage);
    return t1.multiply (t2);
}

private Number calc_term (MathEquation equation, Number pmt, Number fv, Number pint)
{
    /*  Term  - pmt (periodic payment).
     *          fv  (future value).
     *          pint (periodic interest rate).
     *
     *          RESULT = log (1 + (fv * pint / pmt)) / log (1 + pint)
     */

    var t1 = pint.add (new Number.integer (1));
    var t2 = t1.ln ();
    t1 = fv.multiply (pint);
    var t3 = t1.divide (pmt);
    var t4 = t3.add (new Number.integer (1));
    t1 = t4.ln ();
    return t1.divide (t2);
}
