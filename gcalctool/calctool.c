
/*  $Header$
 *
 *  Copyright (c) 1987-2003 Sun Microsystems, Inc. All Rights Reserved.
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

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <sys/types.h>
#include "calctool.h"

time_t time();

static void init_text();

double max_fix[MAXBASES] = {
    6.871947674e+10, 3.245185537e+32, 1.000000000e+36, 2.230074520e+43
};

double min_fix[MAXACC][MAXBASES] = {
    { 2.500000000e-1, 3.750000000e-1, 1.000000000e-1,  4.375000000e-1 },
    { 1.250000000e-1, 4.687500000e-2, 1.000000000e-2,  2.734375000e-2 },
    { 6.250000000e-2, 5.859375000e-3, 1.000000000e-3,  1.708984375e-3 },
    { 3.125000000e-2, 7.324218750e-4, 1.000000000e-4,  1.068115234e-4 },
    { 1.562500000e-2, 9.155273437e-5, 1.000000000e-5,  6.675720215e-6 },
    { 7.812500000e-3, 1.144409180e-5, 1.000000000e-6,  4.172325134e-7 },
    { 6.906250000e-3, 1.430511475e-6, 1.000000000e-7,  2.607703209e-8 },
    { 1.953125000e-3, 1.788139343e-7, 1.000000000e-8,  1.629814506e-9 },
    { 9.765625000e-4, 2.235174179e-8, 1.000000000e-9,  1.018634066e-10 },
    { 4.882812500e-4, 2.793967724e-9, 1.000000000e-10, 6.366462912e-12 }
};

char *base_str[]  = {          /* Strings for each base value. */
    N_("_Bin"), N_("_Oct"), N_("_Dec"), N_("_Hex")
};

char *calc_res[] = {
    "accuracy", "base", "display", "mode", "showregisters", "trigtype"
};

char *dtype_str[] = {          /* Strings for each display mode value. */
    N_("_Eng"), N_("_Fix"), N_("_Sci")
};

char *mode_str[]  = {          /* Strings for each mode value. */
    N_("BASIC"), N_("FINANCIAL"), N_("SCIENTIFIC")
};

char *mstrs[] = {              /* Mode titles to be added to the titlebar. */
    N_("Basic Mode"), N_("Financial Mode"), N_("Scientific Mode")
};


char *ttype_str[] = {          /* Strings for each trig type value. */
    N_("_Degrees"), N_("_Gradients"), N_("_Radians")
};

char digits[] = "0123456789ABCDEF";
int basevals[4] = { 2, 8, 10, 16 };


/* Various string values read/written as X resources. */

char *Rbstr[MAXBASES]     = { "BIN", "OCT", "DEC", "HEX" };
char *Rdstr[MAXDISPMODES] = { "ENG", "FIX", "SCI" };
char *Rmstr[MAXMODES]     = { "BASIC", "FINANCIAL", "SCIENTIFIC" };
char *Rtstr[MAXTRIGMODES] = { "DEG", "GRAD", "RAD" };

/* Valid keys when an error condition has occured. */
/*                           Clr */
int validkeys[MAXVKEYS]  = { GDK_Delete };

Vars v;            /* Calctool variables and options. */

/*  This table shows the keyboard values that are currently being used:
 *
 *           |  a b c d e f g h i j k l m n o p q r s t u v w x y z
 *-----------+-----------------------------------------------------
 *  Lower:   |  a b c d e f     i     l m n   p   r s t u v     y
 *  Upper:   |  A   C D E F G     J K L   N   P   R S T       X Y
 *  Numeric: |  0 1 2 3 4 5 6 7 8 9
 *  Other:   |  @ . + - * / = % ( ) # < > [ ] { } | & ~ ^ ? ! :
 *           |  BackSpace Delete Return
 *-----------+-----------------------------------------------------
 */

struct button b_buttons[B_NOBUTTONS] = {   /* Basic mode button values. */

/* str
   hstr
   mods
   value
   func_char
   menutype
   func
 */

/* Row 1. */
{
    N_("7"),
    N_(
      "Numeric 7\n"
      "\n"
      "Keyboard equivalent:   7\n"
      "\n"
      "This key is inactive in binary base."
    ),
    0,
    GDK_7,
    '7',
    M_NONE,
    do_number
},
{
    N_("8"),
    N_(
      "Numeric 8\n"
      "\n"
      "Keyboard equivalent:   8\n"
      "\n"
      "This key is inactive in binary or octal base."
    ),
    0,
    GDK_8,
    '8',
    M_NONE,
    do_number
},
{
    N_("9"),
    N_(
      "Numeric 9\n"
      "\n"
      "Keyboard equivalent:   9\n"
      "\n"
      "This key is inactive in binary or octal base."
    ),
    0,
    GDK_9,
    '9',
    M_NONE,
    do_number
},
{    
    N_("/"),
    N_(
      "/\n"
      "\n"
      "Division\n"
      "\n"
      "Keyboard equivalent:   /\n"
      "\n"
      "This key takes the last number entered and "
      "divides it by the next number entered."
    ),
    0,
    GDK_slash,
    '/',
    M_NONE,
    do_calc
},
{
    "    ",
    "    ",
    0,
    0,
    ' ',
    M_NONE,
    do_none
},
{
    N_("Bsp"),
    N_(
      "Bsp\n"
      "\n"
      "Erases characters one at a time.\n"
      "\n"
      "Keyboard equivalent:   Back Space\n"
      "\n"
      "This removes the right-most character of the "
      "displayed value and recalculates the value of "
      "the display.\n"
      "\n"
      "Internal accuracy is lost with this operation."
    ),
    0,
    GDK_BackSpace,
    '\010',
    M_NONE,
    do_delete
},
{
    N_("CE"),
    N_(
      "CE\n"
      "\n"
      "Clears the current entry.\n"
      "\n"
      "Keyboard equivalent:   Control-Back Space\n"
      "\n"
      "This clears the value currently displayed."
    ),
    GDK_CONTROL_MASK,
    GDK_BackSpace,
    '\013',
    M_NONE,
    do_clear_entry
},
{
    N_("Clr"),
    N_(
      "Clr\n"
      "\n"
      "Clears the calculator.\n"
      "\n"
      "Keyboard equivalent:   Delete\n"
      "\n"
      "This clears the current display and any partial current calculation."
    ),
    0,
    GDK_Delete,
    '\177',
    M_NONE,
    do_clear
},

/* Row 2. */
{
    N_("4"),
    N_(
      "Numeric 4\n"
      "\n"
      "Keyboard equivalent:   4\n"
      "\n"
      "This key is inactive in binary base."
    ),
    0,
    GDK_4,
    '4',
    M_NONE,
    do_number
},
{
    N_("5"),
    N_(
      "Numeric 5\n"
      "\n"
      "Keyboard equivalent:   5\n"
      "\n"
      "This key is inactive in binary base."
    ),
    0,
    GDK_5,
    '5',
    M_NONE,
    do_number
},
{
    N_("6"),
    N_(
      "Numeric 6\n"
      "\n"
      "Keyboard equivalent:   6\n"
      "\n"
      "This key is inactive in binary base."
    ),
    0,
    GDK_6,
    '6',
    M_NONE,
    do_number
},
{
    N_("*"),
    N_(
      "*\n"
      "\n" 
      "Multiplication\n"
      "\n"
      "Keyboard equivalent:   * or x\n"
      "\n"
      "This key takes the last number entered and "
      "multiplies it by the next number entered."
    ),
    0,
    GDK_asterisk,
    '*',
    M_NONE,
    do_calc
},
{
    N_("Acc"),
    N_(
      "Acc\n"
      "\n"
      "Accuracy\n"
      "\n"
      "Keyboard equivalent:   A\n"
      "\n"
      "Sets the accuracy of both the display and the "
      "memory registers.\n"
      "\n"
      "Click on the Acc button to display a menu of levels "
      "of accuracy. Choose a level from the menu to set "
      "the precision for the display and registers."
    ),
    GDK_SHIFT_MASK,
    GDK_A,
    'A',
    M_ACC,
    do_pending
},
{
    N_("+/-"),
    N_(
      "+/-\n"
      "\n"
      "Change sign.\n"
      "\n"
      "Keyboard equivalent:   C\n"
      "\n"
      "Changes the arithmetic sign of the current "
      "displayed value or the exponent being entered."
    ),
    GDK_SHIFT_MASK,
    GDK_C,
    'C',
    M_NONE,
    do_immed
},
{
    N_("Int"),
    N_(
      "Int\n"
      "\n"
      "Integer portion\n"
      "\n"
      "Keyboard equivalent:   i\n"
      "\n"
      "Returns the integer portion of the current "
      "displayed value."
    ),
    0,
    GDK_i,
    'i',
    M_NONE,
    do_portion
},
{
    N_("Rcl"),
    N_(
      "Rcl\n"
      "\n"
      "Retrieves a memory register.\n"
      "\n"
      "Keyboard equivalent:   R\n"
      "\n"
      "Click on the Rcl button to display a menu of available "
      "registers. Choose a register from the menu to "
      "retrieve its value.\n"
      "\n"
      "When you know the number of the register you "
      "want, you can click SELECT on Rcl, then on the "
      "register's number (a digit from 0 to 9)."
    ),
    GDK_SHIFT_MASK,
    GDK_R,
    'R',
    M_RCL,
    do_pending
},

/* Row 3. */
{
    N_("1"),
    N_(
      "Numeric 1\n"
      "\n"
      "Keyboard equivalent:   1"
    ),
    0,
    GDK_1,
    '1',
    M_NONE,
    do_number
},
{
    N_("2"),
    N_(
      "Numeric 2\n"
      "\n"
      "Keyboard equivalent:   2\n"
      "\n"
      "This key is inactive in binary base."
    ), 
    0, 
    GDK_2,
    '2',
    M_NONE,
    do_number
},    
{     
    N_("3"),
    N_(
      "Numeric 3\n"
      "\n"
      "Keyboard equivalent:   3\n"
      "\n"
      "This key is inactive in binary base."
    ),
    0,
    GDK_3,
    '3',
    M_NONE,
    do_number
},
{
    N_("-"),
    N_(
      "-\n"
      "\n"
      "Subtraction\n"
      "\n"
      "Keyboard equivalent:   -\n"
      "\n"
      "This operation takes the last number entered and "
      "subtracts from it the next number entered."
    ),
    0,
    GDK_minus,
    '-',
    M_NONE,
    do_calc
},
{
    N_("%"),
    N_(
      "%\n"
      "\n" 
      "Percentage\n"
      "\n"
      "Keyboard equivalent:   %\n"
      "\n"
      "Takes the last number entered and calculates a "
      "percentage using the next number entered.\n"
      "\n"
      "Example:\n"
      "\n"
      "        200 % 20 = (returns 40)\n"
      "\n"
      "The calculator is multiplying (200*20)*0.01."
    ),
    GDK_SHIFT_MASK,
    GDK_percent,
    '%',
    M_NONE,
    do_calc
},
{
    N_("Sqrt"),
    N_(
      "Sqrt\n"
      "\n"
      "Square root\n"
      "\n"
      "Keyboard equivalent:   s\n"
      "\n"
      "Calculates the square root of the current "
      "displayed value."
    ),   
    0,   
    GDK_s,
    's',
    M_NONE,
    do_immed
},
{
    N_("Frac"),
    N_(
      "Frac\n"
      "\n"
      "Fractional portion\n"
      "\n"
      "Keyboard equivalent:   :\n"
      "\n"
      "Returns the fractional portion of the current "
      "displayed value."
    ),
    GDK_SHIFT_MASK,
    GDK_colon,
    ':',
    M_NONE,
    do_portion
},
{
    N_("Sto"),
    N_(
      "Sto\n"
      "\n"
      "Stores a value in a memory register.\n"
      "\n"
      "Keyboard equivalent:   S\n"
      "\n"
      "Click on the Sto button to display a menu of all registers. "
      "Choose a register from the menu to store the "
      "displayed value.\n"
      "\n"
      "Alternately, click SELECT on Sto, then on a digit "
      "in the range 0 to 9 to indicate the memory "
      "register in which to store the displayed value."
    ),
    GDK_SHIFT_MASK,
    GDK_S,
    'S',
    M_STO,
    do_pending
},

/* Row 4. */
{
    N_("0"),
    N_(
      "Numeric 0\n"
      "\n"
      "Keyboard equivalent:   0\n"
    ),
    0,
    GDK_0,
    '0',
    M_NONE,
    do_number
},
{    
    N_("."),
    N_(
      ".\n"
      "\n"
      "Numeric point\n"
      "\n"
      "Keyboard equivalent:   .\n"
      "\n"
      "Starts the fractional part of a numeric entry."
    ),
    0,
    GDK_period,
    '.',
    M_NONE,
    do_point
},
{
    N_("="),
    N_(
      "=\n"
      "\n"
      "Calculates a result.\n"
      "\n"
      "Keyboard equivalent:   = or Return\n"
      "\n"
      "Displays the result of the current calculation in "
      "the current base."
    ),
    0,
    GDK_equal,
    '=',
    M_NONE,
    do_calc
},
{
    N_("+"),
    N_(
      "+\n"
      "\n"
      "Addition\n"
      "\n"
      "Keyboard equivalent:   +\n"
      "\n"
      "Takes the last number entered and adds it to the "
      "next number entered."
    ),
    GDK_SHIFT_MASK,
    GDK_plus,
    '+',
    M_NONE,
    do_calc
},
{
    N_("1/x"),
    N_(
      "1/x\n"
      "\n"
      "Reciprocal\n"
      "\n"
      "Keyboard equivalent:   r\n"
      "\n"
      "Returns the value 1 divided by the current "
      "displayed value."
    ),
    0,
    GDK_r,
    'r',
    M_NONE,
    do_immed
},
{
    N_("x^2"),
    N_(
      "x^2\n"
      "\n"
      "Keyboard equivalent:   @\n"
      "\n"
      "Returns the square of the current displayed value."
    ),
    GDK_SHIFT_MASK,
    GDK_at,
    '@',
    M_NONE,
    do_immed
},
{
    N_("Abs"),
    N_(
      "Abs\n"
      "\n"
      "Absolute value.\n"
      "\n"
      "Keyboard equivalent:   u\n"
      "\n"
      "Returns the absolute value of the current "
      "displayed value."
    ),
    0,
    GDK_u,
    'u',
    M_NONE,
    do_portion
},
{
    N_("Exch"),
    N_(
      "Exch\n"
      "\n"
      "Register exchange\n"
      "\n"
      "Keyboard equivalent:   X\n"
      "\n"
      "Click on the Exch button to display the menu of available "
      "registers. You can exchange the contents of a "
      "register with the current displayed value.\n"
      "\n"
      "Alternatively, click SELECT on Exch, then on a "
      "digit from 0 - 9 to indicate which register you "
      "want to exchange with the current display.\n"
      "\n"
      "Until you store a value in a register, it has the "
      "value 0.00. You can also fill a register by "
      "editing the ~/.gcalctoolcf file."
    ),
    GDK_SHIFT_MASK,
    GDK_X,
    'X',
    M_EXCH,
    do_pending
},
};

struct button f_buttons[F_NOBUTTONS] = {   /* Financial mode button values. */

/* str
   hstr
   mods
   value
   func_char
   menutype
   func
 */

/* Row 1. */

{
    N_("Ctrm"),
    N_(
      "Ctrm\n"
      "\n"
      "Compounding term\n"
      "\n"
      "Keyboard equivalent:   m\n"
      "\n"
      "Computes the number of compounding periods it "
      "will take an investment of present value pv to "
      "grow to a future value of fv, earning a fixed "
      "interest rate int per compounding period.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - int (periodic interest rate)\n"
      "   Register 1 - fv  (future value)\n"
      "   Register 2 - pv  (present value)\n"
      "\n"
      "Example:\n"
      "\n"
      "You have just deposited $8,000 in an account "
      "that pays an annual interest rate of 9%, "
      "compounded monthly. You want to determine how "
      "long it will take to double you investment.\n"
      "\n"
      "   Register 0 - 0.0075 (interest rate = 9% / 12)\n"
      "   Register 1 - 16000  (future value).\n"
      "   Register 2 - 8000   (present value).\n"
      "\n"
      "Pressing SELECT on Ctrm returns 92.77, which "
      "tells you that it would take 92.77 months, or "
      "almost eight years, to double your $8,000."
    ),
    0,
    GDK_m,
    'm',
    M_NONE,
    do_business
},
{
    N_("Ddb"),
    N_(
      "Ddb\n"
      "\n"
      "Double-declining depreciation\n"
      "\n"
      "Keyboard equivalent:   d\n"
      "\n"
      "Computes the depreciation allowance on an asset "
      "for a specified period of time, using the "
      "double-declining balance method.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - cost    (amount paid for asset)\n"
      "   Register 1 - salvage (value of asset at end of its life)\n"
      "   Register 2 - life    (useful life of the asset)\n"
      "   Register 3 - period  (time period for depreciation allowance)\n"
      "\n"
      "Example:\n"
      "\n"
      "You have just purchased an office machine for "
      "$8,000. The useful life of this machine is "
      "six years. The salvage value after six years is $900.\n"
      "\n"
      "To compute the depreciate expense for "
      "the fourth year, using the double-declining balance method.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 8000     (amount paid for asset)\n"
      "   Register 1 - 900      (value of asset at end of its life)\n"
      "   Register 2 - 6        (useful life of the asset)\n"
      "   Register 3 - 4        (time period for depreciation allowance)\n"
      "\n"
      "Pressing SELECT on Ddb returns 790.12, which "
      "tells you that the depreciation expense for the "
      "fourth year will be $790.12."
    ),
    GDK_SHIFT_MASK,
    GDK_D,
    'D',
    M_NONE,
    do_business
},
{
    N_("Fv"),
    N_(
      "Fv\n"
      "\n"
      "Future value\n"
      "\n"
      "Keyboard equivalent:   v\n"
      "\n"
      "This calculation determines the future value of "
      "an investment. It computes the future value based "
      "on a series of equal payments, each of amount pmt, "
      "earning periodic interest rate int, over the "
      "number of payment periods in term.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - pmt (periodic payment)\n"
      "   Register 1 - int (periodic interest rate)\n"
      "   Register 2 - n   (number of periods)\n"
      "\n"
      "Example:\n"
      "\n"
      "You plan to deposit $4,000 each year for the next "
      "20 years into a bank account. The account is "
      "paying 8% interest, compounded annually. "
      "Interest is paid on the last day of each year.\n"
      "\n"
      "You want to compute the value of your account "
      "in 20 years. You make each year's contribution "
      "on the last day of the year.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 4000 (periodic payment)\n"
      "   Register 1 - 0.08 (periodic interest rate is 8%)\n"
      "   Register 2 - 20   (number of periods)\n"
      "\n"
      "Clicking SELECT on Fv returns 183047.86, the value "
      "of your account in dollars at the end of 20 years."
    ),
    0,
    GDK_v,
    'v',
    M_NONE,
    do_business
},
{
    N_("Pmt"),
    N_(
      "Pmt\n"
      "\n"
      "Periodic payment\n"
      "\n"
      "Keyboard equivalent:   P\n"
      "\n"
      "Computes the amount of the periodic payment "
      "of a loan. Most installment loans are computed "
      "like ordinary annuities, in that payments are "
      "made at the end of each payment period.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - prin (principal)\n"
      "   Register 1 - int  (periodic interest rate)\n"
      "   Register 2 - n    (term)\n"
      "\n"
      "Example:\n"
      "\n"
      "You are considering taking out a $120,000 mortgage "
      "for 30 years at an annual interest rate of 11.0%. "
      "You want to determine your monthly repayment.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 120000  (principal).\n"
      "   Register 1 - 0.00916 (periodic interest rate is 11.0% / 12)\n"
      "   Register 2 - 360     (term - 30 x 12)\n"
      "\n"
      "Clicking SELECT on Pmt returns 1142.06, the value "
      "in dollars of your monthly repayment."
    ),
    GDK_SHIFT_MASK,
    GDK_P,
    'P',
    M_NONE,
    do_business
},
{
    N_("Pv"),
    N_(
      "Pv\n"
      "\n"
      "Present value\n"
      "\n"
      "Keyboard equivalent:   p\n"
      "\n"
      "Determines the present value of an investment. It "
      "computes the present value based on a series of "
      "equal payments, each of amount pmt, discounted at "
      "periodic interest rate int, over the number of "
      "periods in term.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - pmt (periodic payment)\n"
      "   Register 1 - int (periodic interest rate)\n"
      "   Register 2 - n   (term)\n"
      "\n"
      "Example:\n"
      "\n"
      "You have just won a million dollars. The prize "
      "is awarded in 20 annual payments of $50,000 each "
      "(a total of $1,000,000 over 20 years). Annual "
      "payments are received at the end of each year.\n"
      "\n"
      "You are given the option of receiving a single "
      "lump-sum payment of $400,000 instead of the "
      "million dollars annuity. You want to find out "
      "which option is worth more in today's dollars.\n"
      "\n"
      "If you were to accept the annual payments of "
      "$50,000, you assume that you would invest the "
      "money at a rate of 9%, compounded annually.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 50000 (periodic payment).\n"
      "   Register 1 - 0.09  (periodic interest rate is 9%)\n"
      "   Register 2 - 20    (term)\n"
      "\n"
      "Clicking SELECT on Pv returns a value of "
      "456427.28, which tells you that the $1,000,000 "
      "paid over 20 years is worth $456,427.28 in "
      "present dollars.\n"
      "\n"
      "Based on your assumptions, the lump-sum "
      "payment of $400,000 is worth less than the "
      "million-dollar ordinary annuity, in present "
      "dollars (before taxes)."
    ),
    0,
    GDK_p,
    'p',
    M_NONE,
    do_business
},
{
    N_("Rate"),
    N_(
      "Rate\n"
      "\n"
      "Periodic interest rate\n"
      "\n"
      "Keyboard equivalent:   T\n"
      "\n"
      "Returns the periodic interest necessary for a "
      "present value of pv to grow to a future value of "
      "fv over the number of compounding periods in term.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - fv (future value).\n"
      "   Register 1 - pv (present value).\n"
      "   Register 2 - n  (term).\n"
      "\n"
      "Example:\n"
      "\n"
      "You have invested $20,000 in a bond. The bond "
      "matures in five years, and has a maturity value "
      "of $30,000. Interest is compounded monthly. You "
      "want to determine the periodic interest rate for "
      "this investment.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 30000 (future value)\n"
      "   Register 1 - 20000 (present value)\n"
      "   Register 2 - 60    (term - 5 x 12)\n"
      "\n"
      "Clicking SELECT on Rate returns .00678, which "
      "tells you that the periodic (monthly) interest "
      "rate is 0.678%, under 1% per month.\n"
      "\n"
      "To determine the annual rate, multiply the above "
      "formula by 12, which yields a result of 8.14%."
    ),
    GDK_SHIFT_MASK,
    GDK_T,
    'T',
    M_NONE,
    do_business
},
{
    N_("Sln"),
    N_(
      "Sln\n"
      "\n"
      "Straight-line depreciation\n"
      "\n"
      "Keyboard equivalent:   l\n"
      "\n"
      "Computes the straight-line depreciation of an "
      "asset for one period.\n"
      "\n"
      "The straight-line method of depreciation divides "
      "the depreciable cost (cost - salvage) evenly over "
      "the useful life of an asset. The useful life is "
      "the number of periods (typically years) over "
      "which an asset is depreciated.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - cost     (cost of the asset)\n"
      "   Register 1 - salvage  (salvage value of the asset)\n"
      "   Register 2 - life     (useful life of the asset)\n"
      "\n"
      "Example:\n"
      "\n"
      "You have purchased an office machine for $8,000. "
      "The useful life of this machine is six years, "
      "and the salvage value in eight years will be "
      "$900. You want to compute yearly depreciation "
      "expense, using the straight-line method.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 8000     (cost of the asset)\n"
      "   Register 1 - 900      (salvage value of the asset)\n"
      "   Register 2 - 6        (useful life of the asset)\n"
      "\n"
      "Clicking SELECT on Sln returns 1183.33, the yearly "
      "dollar depreciation allowance."
    ),
    0,
    GDK_l,
    'l',
    M_NONE,
    do_business
},
{ 
    N_("Syd"),
    N_(
      "Syd\n"
      "\n"
      "Sum-of-the years'-digits depreciation\n"
      "\n"
      "Keyboard equivalent:   Y\n"
      "\n"
      "Returns the sum-of-the-years'-digits depreciation "
      "for a specified period.\n"
      "\n"
      "The sum-of-the-years'-digits method of "
      "depreciation accelerates the rate of depreciation, "
      "so that more depreciation expense occurs in "
      "earlier periods than in later ones.\n"
      "\n"
      "The depreciable cost is the actual cost minus "
      "salvage value. The useful life is the number of "
      "periods (typically years) over which an asset is "
      "depreciated.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - cost     (cost of the asset)\n"
      "   Register 1 - salvage  (salvage value of the asset)\n"
      "   Register 2 - life     (useful life of the asset)\n"
      "   Register 3 - period   (period for which depreciation is computed)\n"
      "\n"
      "Example:\n"
      "\n"
      "You have just purchased an office machine for "
      "$8,000. The useful life of this machine is six "
      "years, and the salvage value after eight years "
      "will be $900.\n"
      "\n"
      "You want to compute the depreciation expense for "
      "the fourth year, using the sum-of-the-years'- "
      "digits method.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 8000    (cost of the asset)\n"
      "   Register 1 - 900     (salvage value of the asset)\n"
      "   Register 2 - 6       (useful life of the asset)\n"
      "   Register 3 - 4       (period for which depreciation is computed)\n"
      "\n"
      "Clicking SELECT on Syd returns 1014.29, the dollar "
      "depreciation allowance for the fourth year."
    ),
    0,
    GDK_Y,
    'Y',
    M_NONE,
    do_business
},

/* Row 2. */
{ 
    N_("Term"),
    N_(
      "Term\n"
      "\n"
      "Payment period\n"
      "\n"
      "Keyboard equivalent:   T\n"
      "\n"
      "Returns the number of payment periods in the term "
      "of an ordinary annuity necessary to accumulate a "
      "future value of fv, earning a periodic interest "
      "rate of int. Each payment is equal to amount pmt.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - pmt (periodic payment)\n"
      "   Register 1 - fv  (future value)\n"
      "   Register 2 - int (periodic interest rate)\n"
      "\n"
      "Example:\n"
      "\n"
      "You deposit $1,800 at the end of each year into "
      "a bank account. Your account earns 11% a year, "
      "compounded annually. You want to determine how "
      "long it will take to accumulate $120,000.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 1800    (periodic payment)\n"
      "   Register 1 - 120000  (future value)\n"
      "   Register 2 - 0.11    (periodic interest rate is 11%)\n"
      "\n"
      "Clicking SELECT on Term returns 20.32, the number "
      "of years it will take to accumulate $120,000 in "
      "your account."
    ),
    0,
    GDK_T,
    'T',
    M_NONE,
    do_business
},
{ 
    "    ",
    "    ",
    0,
    0,
    ' ',
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    0,
    0,
    ' ',
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    0,
    0,
    ' ',
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    0,
    0,
    ' ',
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    0,
    0,
    ' ',
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    0,
    0,
    ' ',
    M_NONE,
    do_none
},
{
    "    ",
    "    ",
    0,
    0,
    ' ',
    M_NONE,
    do_none
},
};

struct button s_buttons[S_NOBUTTONS] = {   /* Scientific mode button values. */

/* str
   hstr
   mods
   value
   func_char
   menutype
   func
*/

/* Row 1. */

{
    N_("<"),
    N_(
      "<\n"
      "\n" 
      "Left shift n\n"
      "\n"
      "Keyboard equivalent:   <\n"
      "\n"
      "Use this key to shift the displayed binary value "
      "a designated number of places to the left. Click "
      "SELECT on the key, then on a digit in the range "
      "0 to f to indicate how many places to shift."
    ),
    GDK_SHIFT_MASK,
    GDK_less,
    '<',
    M_LSHF,
    do_pending
},
{
    N_(">"),
    N_(
      ">\n"
      "\n" 
      "Right shift n\n"
      "\n"
      "Keyboard equivalent:   >\n"
      "\n"
      "Use this key to shift the displayed binary value "
      "a designated number of places to the right. "
      "Click SELECT on the key, then on a digit in the "
      "range 0 to f to indicate how many places to shift."
    ),
    GDK_SHIFT_MASK,
    GDK_greater,
    '>',
    M_RSHF,   
    do_pending
},            
{             
    N_("&16"),
    N_(       
      "&16\n" 
      "\n"    
      "Get a 16-bit unsigned integer.\n"
      "\n"    
      "Keyboard equivalent:   ]\n"
      "\n"    
      "This is a logical function that truncates the "
      "given number and returns a 16-bit unsigned "
      "integer."
    ),        
    0,        
    GDK_bracketright,
    ']',
    M_NONE,   
    do_immed  
},            
{             
    N_("&32"),
    N_(       
      "&32\n" 
      "\n"    
      "Get a 32-bit unsigned integer.\n"
      "\n"
      "Keyboard equivalent:   [\n"
      "\n"
      "This is a logical function that truncates the "
      "given number and returns a 32-bit unsigned "
      "integer."
    ),    
    0,
    GDK_bracketleft,
    '[',
    M_NONE,
    do_immed
},
{
    "    ",
    "    ",
    0,
    0,
    ' ',
    M_NONE,
    do_none
},
{
    N_("("),
    N_(
      "(\n"
      "\n" 
      "Left parenthesis\n"
      "\n"
      "Keyboard equivalent:   (\n"
      "\n"
      "Allows you to group together a set of "
      "calculations. Use with the right parenthesis.\n"
      "\n"
      "Example:\n"
      "\n"
      "        2 * ( 3 + 4 ) = 14\n"
      "\n"
      "contrasted with:\n"
      "\n"
      "        2 * 3 + 4 = 10"
    ),
    GDK_SHIFT_MASK,
    GDK_parenleft,
    '(',
    M_NONE,
    do_paren
},
{
    N_(")"),
    N_(
      ")\n"
      "\n"
      "Right parenthesis\n"
      "\n"
      "Keyboard equivalent:   )\n"
      "\n"
      "Allows you to group together a set of "
      "calculations. Use with the left parenthesis.\n"
      "\n"
      "Example:\n"
      "\n"
      "        2 * ( 3 + 4 ) = 14\n"
      "\n"
      "contrasted with:\n"
      "\n"
      "        2 * 3 + 4 = 10"
    ),
    GDK_SHIFT_MASK,
    GDK_parenright,
    ')',
    M_NONE,
    do_paren
},
{
    "    ",
    "    ",
    0,
    0,
    ' ',
    M_NONE,
    do_none
},

/* Row 2. */
{
    N_("Exp"),
    N_(
      "Exp\n"
      "\n"
      "Enters an exponential number.\n"
      "\n"
      "Keyboard equivalent:   E\n"
      "\n"
      "Starts exponential input. Any numbers typed from "
      "now on are the exponent. If you haven't entered a "
      "mantissa, the calculator uses a mantissa of 1.0."
    ),
    GDK_SHIFT_MASK,
    GDK_E,
    'E',
    M_NONE,
    do_expno
},
{
    N_("Con"),
    N_(
      "Con\n"
      "\n"
      "Constant n\n"
      "\n"
      "Keyboard equivalent:   #\n"
      "\n"
      "Click on the Con button to show the available constants. "
      "Choose a constant from the menu to enter its value "
      "in the display.\n"
      "\n"
      "Alternatively, if you know the number of a "
      "constant, you can click SELECT on Con, then "
      "on a digit from 0 - 9 to indicate the constant "
      "you want to enter.\n"
      "\n"            
      "To add new constants or alter existing ones, "
      "choose \"Enter Constant\" from the menu. This "
      "displays a window in which you type the number, "
      "description, and value of the constant.\n"
      "\n"            
      "Click SELECT on \"Enter Constant\" to store the new "
      "constant in the ~/.gcalctoolcf file."
    ),                
    GDK_SHIFT_MASK,   
    GDK_numbersign,   
    '#',
    M_CON,            
    do_pending
},
{
    N_("Fun"),
    N_(
      "Fun\n"
      "\n"
      "Menu of defined functions\n"
      "\n"
      "Keyboard equivalent:   F\n"
      "\n"
      "Click on the Fun button to display a menu of "
      "functions you have defined. Choose a function from "
      "the menu to execute it. The result will show in "
      "the display.\n"
      "\n"
      "To add new functions or alter existing ones, "
      "choose \"Enter Function\" from the menu. This "
      "displays a window in which you type the number, "
      "description, and value of the function.\n"
      "\n"
      "Click SELECT on \"New Function\" to store the new "
      "function in the ~/.gcalctoolcf file.\n"
      "\n"
      "For the function value, type the keyboard strokes "
      "that calculate your function. For example, to "
      "calculate sine(90), type the Value:\n"
      "\n"
      "        90 \\s\n"
      "\n"
      "When you type the function value, use \\ to "
      "represent the Control key even though KEYS shows "
      "the keyboard equivalent of the Control key as "
      "the character ^ .\n"
      "\n"
      "When you know the number of the function you "
      "want, you can click SELECT on Fun, then on the "
      "function's number."
    ),
    GDK_SHIFT_MASK,
    GDK_F,
    'F',
    M_FUN,
    do_pending
},
{
    N_("e^x"),
    N_(
      "e^x\n"
      "\n"
      "e to the x power\n"
      "\n"
      "Keyboard equivalent:   {\n"
      "\n"
      "Returns e raised to the power of the current displayed value."
    ),
    GDK_SHIFT_MASK,
    GDK_braceleft,
    '{',
    M_NONE,
    do_immed
},
{
    N_("10^x"),
    N_(
      "10^x\n"
      "\n"
      "10 to the x power\n"
      "\n"
      "Keyboard equivalent:   }\n"
      "\n"
      "Returns 10 raised to the power of the current displayed value."
    ),
    GDK_SHIFT_MASK,
    GDK_braceright,
    '}',
    M_NONE,
    do_immed
},       
{        
    N_("y^x"),
    N_(  
      "y^x\n"
      "\n"
      "y to the power of x\n"
      "\n"
      "Keyboard equivalent:   y\n"
      "\n"
      "Takes the last number entered and raises it to "
      "the power of the next number entered.\n"
      "\n"
      "Example:\n"
      "\n"
      "        2 Y 3 = (returns 8)"
    ),   
    0,   
    GDK_y,
    'y',
    M_NONE,
    do_calc
},       
{        
    N_("x!"),
    N_(  
      "x!\n"
      "\n"
      "Factorial\n"
      "\n"
      "Keyboard equivalent:   !\n"
      "\n"
      "Returns the factorial of the current displayed "
      "value. This will only work for positive integer values."
    ), 
    GDK_SHIFT_MASK,
    GDK_exclam,
    '!',
    M_NONE,
    do_immed
},
{
    N_("Rand"),
    N_(
      "Rand\n"
      "\n"
      "Random number\n"
      "\n"
      "Keyboard equivalent:   ?\n"
      "\n"
      "Generates a random number in the range 0.0 to 1.0 "
      "and enters it into the calculator display."
    ),
    GDK_SHIFT_MASK,
    GDK_question,
    '?',
    M_NONE,
    do_immed
},

/* Row 3. */
{
    N_("D"),
    N_(
      "Hex D (decimal 13)\n"
      "\n"
      "Keyboard equivalent:   d\n"
      "\n"
      "Enters the hexadecimal value d in the display.\n"
      "\n"
      "Available only when the current base is "
      "hexadecimal."
    ),
    0,
    GDK_d,
    'd',
    M_NONE,
    do_number
},
{
    N_("E"),
    N_(
      "Hex E (decimal 14)\n"
      "\n"
      "Keyboard equivalent:   e\n"
      "\n"
      "Enters the hexadecimal value e in the display.\n"
      "\n"
      "Available only when the current base is "
      "hexadecimal."
    ),
    0,
    GDK_e,
    'e',
    M_NONE,
    do_number
},
{
    N_("F"),
    N_(
      "Hex F (decimal 15)\n"
      "\n"
      "Keyboard equivalent:   f\n"
      "\n"
      "Enters the hexadecimal value f in the display.\n"
      "\n"
      "Available only when the current base is "
      "hexadecimal."
    ),
    0,
    GDK_f,
    'f',
    M_NONE,
    do_number
},
{
    N_("Cos"),
    N_(
      "Cos\n"
      "\n"
      "Cosine function\n"
      "\n"
      "Keyboard equivalent:   J\n"
      "\n"
      "Returns the trigonometric cosine, arc cosine, "
      "hyperbolic cosine, or inverse hyperbolic cosine "
      "of the current displayed value, depending on the "
      "settings of the Hyp and Inv flags.\n"
      "\n"
      "The result appears in the current trigonometric "
      "unit (degrees, radians, or gradients)."
    ),
    GDK_SHIFT_MASK,
    GDK_J,
    'J',
    M_NONE,
    do_trig
},
{
    N_("Sin"),
    N_(
      "Sin\n"
      "\n"
      "Sine function\n"
      "\n"
      "Keyboard equivalent:   K\n"
      "\n"
      "Returns the trigonometric sine, arc sine, "
      "hyperbolic sine, or inverse hyperbolic sine of "
      "the current displayed value, depending on the "
      "settings of the Hyp and Inv flags.\n"
      "\n"
      "The result appears in the current trigonometric "
      "unit (degrees, radians, or gradients)."
    ),   
    GDK_SHIFT_MASK,
    GDK_K,
    'K',
    M_NONE,
    do_trig
},
{        
    N_("Tan"),
    N_(  
      "Tan\n"
      "\n"
      "Tangent function\n"
      "\n"
      "Keyboard equivalent:   L\n"
      "\n"
      "Returns the trigonometric tangent, arc tangent, "
      "hyperbolic tangent, or inverse hyperbolic tangent "
      "of the current displayed value, depending on the "
      "settings of the Hyp and Inv flags.\n"
      "\n"
      "The result appears in the current trigonometric "
      "unit (degrees, radians, or gradients)."
    ), 
    GDK_SHIFT_MASK,
    GDK_L,
    'L',
    M_NONE,
    do_trig
},     
{      
    N_("Ln"),
    N_(
      "Ln\n"
      "\n"
      "Natural log\n"
      "\n"
      "Keyboard equivalent:   N\n"
      "\n"
      "Returns the natural logarithm of the current displayed value."
    ),
    GDK_SHIFT_MASK,
    GDK_N,
    'N',
    M_NONE,
    do_immed
},
{ 
    N_("Log"),
    N_(
      "Log\n"
      "\n"
      "Base 10 log\n"
      "\n"
      "Keyboard equivalent:   G\n"
      "\n"
      "Returns the base 10 logarithm of the current displayed value."
    ),
    GDK_SHIFT_MASK,
    GDK_G,
    'G',
    M_NONE,
    do_immed
},

/* Row 4. */
{
    N_("A"),
    N_(
      "Hex A (decimal 10)\n"
      "\n"
      "Keyboard equivalent:   a\n"
      "\n"
      "Enters the hexadecimal value a in the display.\n"
      "\n"
      "Available only when the current base is "
      "hexadecimal."
    ),
    0,
    GDK_a,
    'a',
    M_NONE,
    do_number
},
{
    N_("B"),
    N_(
      "Hex B (decimal 11)\n"
      "\n"
      "Keyboard equivalent:   b\n"
      "\n"
      "Enters the hexadecimal value b in the display.\n"
      "\n"
      "Available only when the current base is "
      "hexadecimal."
    ),
    0,
    GDK_b,
    'b',
    M_NONE,
    do_number
},    
{     
    N_("C"),
    N_(
      "Hex C (decimal 12)\n"
      "\n"
      "Keyboard equivalent:   c\n"
      "\n"
      "Enters the hexadecimal value c in the display.\n"
      "\n"
      "Available only when the current base is "
      "hexadecimal."
    ),
    0,
    GDK_c,
    'c',
    M_NONE,
    do_number
},
{
    N_("Or"),
    N_(
      "Or\n"
      "\n"
      "Logical OR\n"
      "\n"
      "Keyboard equivalent:   |\n"
      "\n"
      "This key performs a logical OR operation on the "
      "last number entered and the next number entered, "
      "treating both numbers as unsigned long integers."
    ),
    GDK_SHIFT_MASK,
    GDK_bar,
    '|',
    M_NONE,
    do_calc
},
{
    N_("And"),
    N_(
      "And\n"
      "\n"
      "Logical AND\n"
      "\n"
      "Keyboard equivalent:   &\n"
      "\n"
      "This key performs a logical AND operation on the "
      "last number entered and the next number entered, "
      "treating both numbers as unsigned long integers."
    ),
    GDK_SHIFT_MASK,
    GDK_ampersand,
    '&',
    M_NONE,
    do_calc
},       
{        
    N_("Not"),
    N_(  
      "Not\n"
      "\n"
      "Logical NOT\n"
      "\n"
      "Keyboard equivalent:   ~\n"
      "\n"
      "This key performs a logical NOT operation on the "
      "current displayed value."
    ),   
    GDK_SHIFT_MASK,
    GDK_asciitilde,
    '~',
    M_NONE,
    do_immed
},
{
    N_("Xor"),
    N_(
      "Xor\n"
      "\n"
      "Logical XOR\n"
      "\n"
      "Keyboard equivalent:   ^\n"
      "\n"
      "This key takes the last number entered and the "
      "next number entered, and performs a logical XOR "
      "operation on them, treating both numbers as "
      "unsigned long integers."
    ),
    GDK_SHIFT_MASK,
    GDK_caret,
    '^',
    M_NONE,
    do_calc
},
{
    N_("Xnor"),
    N_(
      "Xnor\n"
      "\n"
      "Logical XNOR\n"
      "\n"
      "Keyboard equivalent:   n\n"
      "\n"
      "This key takes the last number entered and the "
      "next number entered, and performs a logical XNOR "
      "operation on them, treating both numbers as "
      "unsigned long integers."
    ),
    0,
    GDK_n,
    'n',
    M_NONE,
    do_calc
},
};


void
do_calctool(int argc, char **argv)
{
    char *ptr, title[MAXLINE];
    int i;

    v->progname = argv[0];     /* Save programs name. */
    v->appname  = NULL;

    if ((ptr = strrchr(argv[0], '/')) != NULL) {
        read_str(&v->appname, ptr+1);
    } else {
        read_str(&v->appname, argv[0]);
    }

/*  Search through all the command line arguments, looking for -name.
 *  If it's present, then this name with be used, when looking for X resources
 *  for this application. When the rest of the command line arguments are
 *  checked later on, then the -name argument (if found) is ignored.
 */

    for (i = 0; i < argc; i++) {
        if (EQUAL(argv[i], "-name")) {
            if ((i+1) > argc) {
                usage(v->progname);
            }
            read_str(&v->appname, argv[i+1]);
            break;
        }
    }

    init_text();               /* Setup text strings depending upon language. */
    init_vars();               /* Setup default values for variables. */
    load_resources();          /* Get resources from various places. */
    read_resources();          /* Read resources from merged database. */
    get_options(argc, argv);   /* Get command line arguments. */
    read_rcfiles();            /* Read .calctoolrc's files. */
    make_frames();             /* Create gcalctool window frames. */

    v->current    = copy_button_info(button_for_value(KEY_EQ));
    v->shelf      = NULL;      /* No selection for shelf initially. */
    v->noparens   = 0;         /* No unmatched brackets initially. */
    v->opsptr     = 0;         /* Nothing on the parentheses op stack. */
    v->numsptr    = 0;         /* Nothing on the parenthese numeric stack. */
    v->pending    = 0;         /* No initial pending command. */
    v->hyperbolic = 0;         /* Normal trig functions initially. */
    v->inverse    = 0;         /* No inverse functions initially. */
    v->down       = 0;         /* No mouse presses initially. */

    srand48((long) time((time_t *) 0));   /* Seed random number generator. */

    do_clear();                /* Initialise and clear display. */

    if (v->rstate == TRUE) {   /* Show the memory register window? */
        make_registers();
        if (!v->iconic) win_display(FCP_REG, TRUE);
    }

    SPRINTF(title, "%s   [%s]", v->tool_label, mstrs[(int) v->modetype]);
    set_title(FCP_KEY, title);

    show_display(v->MPdisp_val);     /* Output in correct display mode. */
    start_tool();                    /* Display the calculator. */
}


/* Calctools' customised math library error-handling routine. */

void
doerr(char *errmes)
{
    if (!v->started) {
        return;
    }
    STRCPY(v->display, errmes);
    set_display(v->display);
    v->error = 1;
    beep();
}


static void
init_text()         /* Setup constant strings. */
{
    STRCPY(v->con_names[0], _("kilometres per hour <=> miles per hour."));
    STRCPY(v->con_names[1], _("square root of 2."));
    STRCPY(v->con_names[2], _("e."));
    STRCPY(v->con_names[3], _("pi."));
    STRCPY(v->con_names[4], _("centimetres <=> inch."));
    STRCPY(v->con_names[5], _("degrees in a radian."));
    STRCPY(v->con_names[6], _("2 ^ 20."));
    STRCPY(v->con_names[7], _("grams <=> ounce."));
    STRCPY(v->con_names[8], _("kilojoules <=> British thermal units."));
    STRCPY(v->con_names[9], _("cubic cms <=> cubic inches."));
}


/* Default math library exception handling routine. */

/*ARGSUSED*/
int
matherr(exc)
struct exception *exc;
{
    doerr(_("Error"));

    return(1);
}
