
/*  $Header$
 *
 *  Copyright (c) 1987-2006 Sun Microsystems, Inc. All Rights Reserved.
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
#include <stdlib.h>
#include <sys/types.h>
#include "calctool.h"
#include <gdk/gdkkeysyms.h>

#include "functions.h"

time_t time();

static void init_buttons();
static void init_text();

double max_fix[MAXBASES] = {
    6.871947674e+10, 3.245185537e+32, 1.000000000e+36, 2.230074520e+43
};

char *base_str[]  = {          /* Strings for each base value. */
    N_("_Bin"), N_("_Oct"), N_("_Dec"), N_("He_x")
};

char *base_desc[]  = {         /* Tooltips for each base value. */
    N_("Set numeric base to binary (base 2)"), 
    N_("Set numeric base to octal (base 8)"), 
    N_("Set numeric base to decimal (base 10)"), 
    N_("Set numeric base to hexadecimal (base 16)")
};

char *calc_res[] = {
    "accuracy", "base", "display", "modetype", "showregisters", "trigtype",
    "showzeroes", "showthousands", "syntax", "xposition", "yposition",
    "register0", "register1", "register2", "register3", "register4",
    "register5", "register6", "register7", "register8", "register9", "bitcalculating"
};

char *dtype_str[] = {          /* Strings for each display mode value. */
    N_("E_ng"), N_("_Fix"), N_("_Sci")
};

char *dtype_desc[] = {         /* Tooltips for each display mode value. */
    N_("Set display type to engineering format"), 
    N_("Set display type to fixed-point format"), 
    N_("Set display type to scientific format")
};

char *hyp_desc = N_("Set hyperbolic option for trigonometric functions");
char *inv_desc = N_("Set inverse option for trigonometric functions");

char *mstrs[] = {              /* Mode titles to be added to the titlebar. */
    N_("Basic"), N_("Advanced"), N_("Financial"), 
    N_("Scientific"), N_("Expression")
};

char *ttype_str[] = {          /* Strings for each trig type value. */
    N_("De_grees"), N_("Gr_adians"), N_("_Radians")
};

char *ttype_desc[] = {         /* Tooltips for each trig type value. */
    N_("Set trigonometric type to degrees"), 
    N_("Set trigonometric type to gradians"), 
    N_("Set trigonometric type to radians")
};

char digits[] = "0123456789ABCDEF";
int basevals[4] = { 2, 8, 10, 16 };


/* Various string values read/written as X resources. */

char *Rbstr[MAXBASES]     = { "BIN", "OCT", "DEC", "HEX" };
char *Rdstr[MAXDISPMODES] = { "ENG", "FIX", "SCI" };
char *Rmstr[MAXMODES]     = { "BASIC", "ADVANCED", "FINANCIAL", 
                              "SCIENTIFIC" };
char *Rtstr[MAXTRIGMODES] = { "DEG", "GRAD", "RAD" };
char *Rsstr[MAXSYNTAX]    = { "ARITHMETIC", "ARITHMETIC_PRECEDENCE" };
char *Rcstr[MAXBITCALC]   = { "NO_BITCALCULATING_MODE", "BITCALCULATING_MODE" };


/* Valid keys when an error condition has occured. */
/*                           Clr */
int validkeys[MAXVKEYS]  = { GDK_Delete };

Vars v;            /* Calctool variables and options. */

/*  This table shows the keyboard values that are currently being used:
 *
 *           |  a b c d e f g h i j k l m n o p q r s t u v w x y z
 *-----------+-----------------------------------------------------
 *  Lower:   |  a b c d e f     i     l m n   p   r s t u v   x
 *  Upper:   |  A   C D E F G     J K L M N   P   R S T       X Y
 *  Numeric: |  0 1 2 3 4 5 6 7 8 9
 *  Other:   |  @ . + - * / = % ( ) # < > [ ] { } | & ~ ^ ? ! :
 *           |  BackSpace Delete Return
 *-----------+-----------------------------------------------------
 */

struct button b_buttons[B_NOBUTTONS];      /* Buttons for "Basic" mode. */

struct button a_buttons[A_NOBUTTONS] = {   /* Advanced mode button values. */

/* str
   hstr
   astr,
   mods
   value
   func_char
   menutype
   func
 */

/* Row 1. */
{
    N_("7"),
    NULL,
    N_("Numeric 7"),
    { 0,     GDK_SHIFT_MASK, 0,        0,           0,      0 },
    { GDK_7, GDK_7,          GDK_KP_7, GDK_KP_Home, GDK_R7, 0 },
    '7',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    N_("8"),
    NULL,
    N_("Numeric 8"),
    { 0,     GDK_SHIFT_MASK, 0,        0,         0 },
    { GDK_8, GDK_8,          GDK_KP_8, GDK_KP_Up, 0 },
    '8',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    N_("9"),
    NULL,
    N_("Numeric 9"),
    { 0,     GDK_SHIFT_MASK, 0,        0,              0,      0 },
    { GDK_9, GDK_9,          GDK_KP_9, GDK_KP_Page_Up, GDK_R9, 0 },
    '9',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    /* Note to translators: this is a division sign (÷) */
    N_("\303\267"),
    N_("Divide"),
    NULL,
    { 0,         GDK_SHIFT_MASK, 0,             0,      GDK_SHIFT_MASK, 0 },
    { GDK_slash, GDK_slash,      GDK_KP_Divide, GDK_R5, GDK_slash,      0 },
    '/',
    M_NONE,
    do_calc,
    N_("/"),
    binop
},
{
    N_("("),
    N_("Start group of calculations"),
    N_("Left bracket"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_parenleft, 0 },
    '(',
    M_NONE,
    do_paren,
    NULL,
    parenthesis
},
{
    /* Note to translators: Bksp is short for Backspace. */
    N_("Bksp"),
    N_("Remove rightmost character from displayed value"),
    N_("Backspace"),
    { 0, 0 },
    { GDK_BackSpace, 0 },
    '\010',
    M_NONE,
    do_delete,
    NULL,
    bsp
},
{
    N_("CE"),
    N_("Clear displayed value"),
    N_("Clear entry"),
    { GDK_CONTROL_MASK, 0,          0 },
    { GDK_BackSpace,    GDK_Escape, 0 },
    '\013',
    M_NONE,
    do_clear_entry,
    NULL,
    clear
},
{
    /* Note to translators: Clr is short for Clear. */
    N_("Clr"),
    N_("Clear displayed value and any partial calculation"),
    N_("Clear"),
    { 0, 0 },
    { GDK_Delete, 0 },
    '\177',
    M_NONE,
    do_clear,
    NULL,
    clear
},

/* Row 2. */
{
    N_("4"),
    NULL,
    N_("Numeric 4"),
    { 0,     GDK_SHIFT_MASK, 0,        0,           0 },
    { GDK_4, GDK_4,          GDK_KP_4, GDK_KP_Left, 0 },
    '4',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    N_("5"),
    NULL,
    N_("Numeric 5"),
    { 0,     GDK_SHIFT_MASK, 0,        0,            0,       0 },
    { GDK_5, GDK_5,          GDK_KP_5, GDK_KP_Begin, GDK_R11, 0 },
    '5',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    N_("6"),
    NULL,
    N_("Numeric 6"),
    { 0,     GDK_SHIFT_MASK, 0,        0,            0 },
    { GDK_6, GDK_6,          GDK_KP_6, GDK_KP_Right, 0 },
    '6',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    /* Note to translators: this is a multiplication sign (*) */
    N_("\303\227"),
    N_("Multiply"),
    NULL,
    { GDK_SHIFT_MASK, 0,               0,     0,      0 },
    { GDK_asterisk,   GDK_KP_Multiply, GDK_x, GDK_R6, 0 },
    '*',
    M_NONE,
    do_calc,
    N_("*"),
    binop
},
{
    N_(")"),
    N_("End group of calculations"),
    N_("Right bracket"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_parenright, 0 },
    ')',
    M_NONE,
    do_paren,
    NULL,
    parenthesis
},
{
    /* Note to translators: this is a plus-minus sign (+/-) */
    N_("\302\261"),
    N_("Change sign [c]"),
    NULL,
    { 0,     0 },
    { GDK_c, 0 },
    'c',
    M_NONE,
    do_immed, 
    N_("Chs"),
    neg
},
{
    N_("Int"),
    N_("Integer portion of displayed value [i]"),
    N_("Integer portion"),
    { 0, 0 },
    { GDK_i, 0 },
    'i',
    M_NONE,
    do_portion,
    NULL,
    func
},
{
    N_("Sto"),
    N_("Store displayed value in memory register [S]"),
    N_("Store to register"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_S, 0 },
    'S',
    M_STO,
    do_pending,
    NULL,
    none
},

/* Row 3. */
{
    N_("1"),
    NULL,
    N_("Numeric 1"),
    { 0,     GDK_SHIFT_MASK, 0,        0,          0,       0 },
    { GDK_1, GDK_1,          GDK_KP_1, GDK_KP_End, GDK_R13, 0 },
    '1',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    N_("2"),
    NULL,
    N_("Numeric 2"),
    { 0,     GDK_SHIFT_MASK, 0,        0,           0 }, 
    { GDK_2, GDK_2,          GDK_KP_2, GDK_KP_Down, 0 },
    '2',
    M_NONE,
    do_number,
    NULL,
    number
},    
{     
    N_("3"),
    NULL,
    N_("Numeric 3"),
    { 0,     GDK_SHIFT_MASK, 0,        0,                0,       0 },
    { GDK_3, GDK_3,          GDK_KP_3, GDK_KP_Page_Down, GDK_R15, 0 },
    '3',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    /* Note to translators: this is a minus sign (-) */
    N_("\342\210\222"),
    N_("Subtract"),
    NULL,
    { 0,         0,               0,      0 },
    { GDK_minus, GDK_KP_Subtract, GDK_R4, 0 },
    '-',
    M_NONE,
    do_calc,
    N_("-"),
    unop | binop
},
{
    N_("%"),
    N_("Percentage"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_percent, 0 },
    '%',
    M_NONE,
    do_percent,  
    NULL,
    immediate
},
{
    /* Note to translators: this is a square root sign */
    N_("\342\210\232"),
    N_("Square root [s]"),
    NULL,
    { 0, 0 },   
    { GDK_s, 0 },
    's',
    M_NONE,
    do_immed, 
    N_("Sqrt"),
    func
},
{
    N_("Frac"),
    N_("Fractional portion of displayed value [:]"),
    N_("Fractional portion"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_colon, 0 },
    ':',
    M_NONE,
    do_portion,
    NULL,
    func  
},
{
    N_("Rcl"),
    N_("Retrieve memory register to display [R]"),
    N_("Retrieve from register"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_R, 0 },
    'R',
    M_RCL,
    do_pending,
    NULL,
    none
},

/* Row 4. */
{
    N_("0"),
    NULL,
    N_("Numeric 0"),
    { 0,     GDK_SHIFT_MASK, 0,        0,             0 },
    { GDK_0, GDK_0,          GDK_KP_0, GDK_KP_Insert, 0 },
    '0',
    M_NONE,
    do_number,
    NULL,
    number
},
{    
    N_("."),
    N_("Numeric point"),
    NULL,
    { 0,          0,              0,             0 },
    { GDK_period, GDK_KP_Decimal, GDK_KP_Delete, GDK_KP_Separator, 0 },
    '.',
    M_NONE,
    do_point,
    NULL,
    number | dpoint
},
{
    N_("="),
    N_("Calculate result"),
    NULL,
    { 0,         0,            0,          GDK_SHIFT_MASK, 0 },
    { GDK_equal, GDK_KP_Enter, GDK_Return, GDK_equal,      0 },
    '=',
    M_NONE,
    do_calc,
    NULL,
    enter
},
{
    N_("+"),
    N_("Add"),
    NULL,
    { GDK_SHIFT_MASK, 0,        0,          0 },
    { GDK_plus,       GDK_plus, GDK_KP_Add, 0 },
    '+',
    M_NONE,
    do_calc,
    NULL,
    binop
},
{
  N_("1/<i>x</i>"),
    N_("Reciprocal [r]"),
    NULL,
    { 0, 0 },
    { GDK_r, 0 },
    'r',
    M_NONE,
    do_immed, 
    N_("Recip"),
    inv
},
{
    N_("<i>x</i><sup>2</sup>"),
    N_("Square [@]"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_at, 0 },
    '@',
    M_NONE,
    do_immed,  
    N_("^2"),
    immediate | postfixop
},
{
    /* Note to translators: Abs is short for Absolute. */
    N_("Abs"),
    N_("Absolute value [u]"),
    NULL,
    { 0, 0 },
    { GDK_u, 0 },
    'u',
    M_NONE,
    do_portion,
    NULL,
    func
},
{
    N_("Exch"),
    N_("Exchange displayed value with memory register [X]"),
    N_("Exchange with register"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_X, 0 },
    'X',
    M_EXCH,
    do_pending,
    NULL,
    none
},
};

struct button f_buttons[F_NOBUTTONS] = {   /* Financial mode button values. */

/* str
   hstr
   astr
   mods
   value
   func_char
   menutype
   func
 */

/* Row 1. */

{
    N_("Ctrm"),
    N_("Compounding term [m]"),
    NULL,
    { 0, 0 },
    { GDK_m, 0 },
    'm',
    M_NONE,
    do_business,
    NULL,
    none
},
{
    N_("Ddb"),
    N_("Double-declining depreciation [d]"),
    NULL,
    { 0,     0 },
    { GDK_d, 0 },
    'd',
    M_NONE,
    do_business,
    NULL,
    none
},
{
    N_("Fv"),
    N_("Future value [v]"),
    NULL,
    { 0, 0 },
    { GDK_v, 0 },
    'v',
    M_NONE,
    do_business,
    NULL,
    none
},
{
    N_("Pmt"),
    N_("Periodic payment [P]"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_P, 0 },
    'P',
    M_NONE,
    do_business,
    NULL,
    none
},
{
    N_("Pv"),
    N_("Present value [p]"),
    NULL,
    { 0, 0 },
    { GDK_p, 0 },
    'p',
    M_NONE,
    do_business,
    NULL,
    none
},
{
    N_("Rate"),
    N_("Periodic interest rate [T]"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_T, 0 },
    'T',
    M_NONE,
    do_business,
    NULL,
    none
},
{
    N_("Sln"),
    N_("Straight-line depreciation [l]"),
    NULL,
    { 0, 0 },
    { GDK_l, 0 },
    'l',
    M_NONE,
    do_business,
    NULL,
    none
},
{ 
    N_("Syd"),
    N_("Sum-of-the years'-digits depreciation [Y]"),
    NULL,
    { 0, 0 },
    { GDK_Y, 0 },
    'Y',
    M_NONE,
    do_business,
    NULL,
    none
},

/* Row 2. */
{ 
    N_("Term"),
    N_("Payment period [T]"),
    NULL,
    { 0, 0 },
    { GDK_T, 0 },
    'T',
    M_NONE,
    do_business,
    NULL,
    none
},
{ 
    "    ",
    "    ",
    NULL,
    { 0, 0 },
    { 0, 0 },
    ' ',
    M_NONE,
    do_none,
    NULL,
    none
},
{ 
    "    ",
    "    ",
    NULL,
    { 0, 0 },
    { 0, 0 },
    ' ',
    M_NONE,
    do_none,
    NULL,
    none
},
{ 
    "    ",
    "    ",
    NULL,
    { 0, 0 },
    { 0, 0 },
    ' ',
    M_NONE,
    do_none,
    NULL,
    none
},
{ 
    "    ",
    "    ",
    NULL,
    { 0, 0 },
    { 0, 0 },
    ' ',
    M_NONE,
    do_none,
    NULL,
    none
},
{ 
    "    ",
    "    ",
    NULL,
    { 0, 0 },
    { 0, 0 },
    ' ',
    M_NONE,
    do_none,
    NULL,
    none
},
{ 
    "    ",
    "    ",
    NULL,
    { 0, 0 },
    { 0, 0 },
    ' ',
    M_NONE,
    do_none,
    NULL,
    none
},
{
    "    ",
    "    ",
    NULL,
    { 0, 0 },
    { 0, 0 },
    ' ',
    M_NONE,
    do_none,
    NULL,
    none
},
};

struct button s_buttons[S_NOBUTTONS] = {   /* Scientific mode button values. */

/* str
   hstr
   astr
   mods
   value
   func_char
   menutype
   func
*/

/* Row 1. */

{
    N_("<"),
    N_("Shift displayed value 1-15 places to the left"),
    N_("Shift left"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_less, 0 },
    '<',
    M_LSHF,
    do_pending,
    NULL,
    none
},
{
    N_(">"),
    N_("Shift displayed value 1-15 places to the right"),
    N_("Shift right"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_greater, 0 },
    '>',
    M_RSHF,   
    do_pending,
    NULL,
    none
},            
{             
    N_("&amp;16"),
    N_("16-bit unsigned integer value of display (])"),
    N_("16 bit unsigned integer"),
    { 0, 0 },        
    { GDK_bracketright, 0 },
    ']',
    M_NONE,   
    do_immed,  
    N_("u16"),
    func
},            
{             
    N_("&amp;32"),
    N_("32-bit unsigned integer value of display ([)"),
    N_("32 bit unsigned integer"),
    { 0, 0 },
    { GDK_bracketleft, 0 },
    '[',
    M_NONE,
    do_immed,  
    N_("u32"),
    func
},
{
    "    ",
    "    ",
    NULL,
    { 0, 0 },
    { 0, 0 },
    ' ',
    M_NONE,
    do_none,
    NULL,
    none
},
{
    "    ",
    "    ",
    NULL,
    { 0, 0 },
    { 0, 0 },
    ' ',
    M_NONE,
    do_none,
    NULL,
    none
},
{
    N_("Mod"),
    N_("Modulus Division"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_M,          0 },
    'M',
    M_NONE,
    do_calc,
    N_(" Mod "),
    binop
},
{
    N_("Acc"),
    N_("Set accuracy from 0 to 9 numeric places [a]"),
    N_("Accuracy"),
    { 0,     0 },
    { GDK_a, 0 },
    'a',
    M_ACC,
    do_pending,
    NULL,
    none
},

/* Row 2. */
{
    N_("Con"),
    N_("Constants [#]"),
    NULL,
    { GDK_SHIFT_MASK, 0,              0 },
    { GDK_numbersign, GDK_numbersign, 0 },   
    '#',
    M_CON,            
    do_pending,
    NULL,
    none
},
{
    N_("Fun"),
    N_("User-defined functions [f]"),
    NULL,
    { 0,     0 },
    { GDK_f, 0 },
    'f',
    M_FUN,
    do_pending,
    NULL,
    none
},
{
    N_("Exp"),
    N_("Enter an exponential number [e]"),
    N_("Exponential"),
    { 0,     0 },
    { GDK_e, 0 },
    'e',
    M_NONE,
    do_expno,
    N_("e"),
    expnum
},
{
    N_("e<sup><i>x</i></sup>"),
    N_("e to the power of displayed value [{]"),
    N_("E to the x"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_braceleft, 0 },
    '{',
    M_NONE,
    do_immed, 
    N_("e^"),
    immediate | prefixop
},
{
    N_("10<sup><i>x</i></sup>"),
    N_("10 to the power of displayed value [}]"),
    N_("Ten to the x"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_braceright, 0 },
    '}',
    M_NONE,
    do_immed, 
    N_("10^"),
    prefixop
},       
{        
    N_("<i>x</i><sup><i>y</i></sup>"),
    N_("Raise displayed value to the power of y [^]"),
    N_("X to the y"),
    { GDK_SHIFT_MASK, GDK_SHIFT_MASK,  0 },
    { GDK_caret,      GDK_asciicircum, 0 },
    '^',
    M_NONE,
    do_calc,
    N_("^"),
    binop | postfixop
},       
{        
    N_("<i>x</i>!"),
    N_("Factorial of displayed value [!]"),
    N_("Factorial"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_exclam, 0 },
    '!',
    M_NONE,
    do_immed, 
    N_("!"),
    immediate | postfixop
},
{
    N_("Rand"),
    N_("Random number in the range 0.0 to 1.0 [?]"),
    N_("Random number"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_question, 0 },
    '?',
    M_NONE,
    do_immed, 
    NULL,
    none
},

/* Row 3. */
{
    N_("D"),
    N_("Hexadecimal digit D"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_D,          0 },
    'D',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    N_("E"),
    N_("Hexadecimal digit E"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_E,          0 },
    'E',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    N_("F"),
    N_("Hexadecimal digit F"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_F,          0 },
    'F',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    N_("Cos"),
    N_("Cosine [J]"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_J, 0 },
    'J',
    M_NONE,
    do_trig,
    NULL,
    func
},
{
    N_("Sin"),
    N_("Sine [K]"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_K, 0 },
    'K',
    M_NONE,
    do_trig,
    NULL,
    func
},
{        
    N_("Tan"),
    N_("Tangent [L]"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_L, 0 },
    'L',
    M_NONE,
    do_trig,
    NULL,  
    func
},     
{      
    N_("Ln"),
    N_("Natural log [N]"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_N, 0 },
    'N',
    M_NONE,
    do_immed, 
    NULL,
    func
},
{ 
    N_("Log"),
    N_("Base 10 log [G]"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_G, 0 },
    'G',
    M_NONE,
    do_immed, 
    NULL,
    func
},

/* Row 4. */
{
    N_("A"),
    N_("Hexadecimal digit A"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_A,          0 },
    'A',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    N_("B"),
    N_("Hexadecimal digit B"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_B,          0 },
    'B',
    M_NONE,
    do_number,
    NULL,
    number
},    
{     
    N_("C"),
    N_("Hexadecimal digit C"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_C,          0 },
    'C',
    M_NONE,
    do_number,
    NULL,
    number
},
{
    N_("Or"),
    N_("Bitwise OR"),
    "bitwise OR [!]",
    { GDK_SHIFT_MASK, 0 },
    { GDK_bar, 0 },
    '|',
    M_NONE,
    do_calc,
    N_(" Or "),
    binop
},
{
    N_("And"),
    N_("Bitwise AND [&]"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_ampersand, 0 },
    '&',
    M_NONE,
    do_calc,
    N_(" And "),
    binop
},       
{        
    N_("Not"),
    N_("Bitwise NOT [~]"),
    NULL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_asciitilde, 0 },
    '~',
    M_NONE,
    do_immed, 
    N_("~"),
    unop | immediate
},
{
    N_("Xor"),
    N_("Bitwise XOR [x]"),
    NULL,
    { 0, 0 },   
    { GDK_x, 0 },
    'x',
    M_NONE,
    do_calc,
    N_(" Xor "),
    binop
},
{
    N_("Xnor"),
    N_("Bitwise XNOR [n]"),
    NULL,
    { 0, 0 },
    { GDK_n, 0 },
    'n',
    M_NONE,
    do_calc,
    N_(" Xnor "),
    binop
},
};


void
do_calctool(int argc, char **argv)
{
    char *ptr;
    int i;

    init_buttons();            /* Setup the buttons for "Basic" mode. */

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

    v->radix = get_radix();    /* Locale specific radix string. */
    v->tsep  = get_tsep();     /* Locale specific thousands seperator. */

    init_text();               /* Setup text strings depending upon language. */
    init_vars();               /* Setup default values for variables. */
    load_resources();          /* Get resources from various places. */
    read_resources();          /* Read resources from merged database. */
    get_options(argc, argv);   /* Get command line arguments. */
    read_cfdefs();             /* Read constant/function definitions. */
    make_frames();             /* Create gcalctool window frames. */

    v->current    = copy_button_info(button_for_value(KEY_EQ.value[0]));
    v->shelf      = NULL;      /* No selection for shelf initially. */
    v->noparens   = 0;         /* No unmatched brackets initially. */
    v->opsptr     = 0;         /* Nothing on the parentheses op stack. */
    v->numsptr    = 0;         /* Nothing on the parenthese numeric stack. */
    v->pending    = 0;         /* No initial pending command. */
    v->hyperbolic = 0;         /* Normal trig functions initially. */
    v->inverse    = 0;         /* No inverse functions initially. */
    v->down       = 0;         /* No mouse presses initially. */
    v->warn_change_mode = 1;   /* Warn user when changing modes. */

    srand48((long) time((time_t *) 0));   /* Seed random number generator. */

    do_clear();                /* Initialise and clear display. */

    if (v->rstate == TRUE) {   /* Show the memory register window? */
        make_registers();
        if (!v->iconic) {
            win_display(FCP_REG, TRUE);
        }
    }

    set_main_title(v->modetype);

    show_display(v->MPdisp_val);     /* Output in correct display mode. */

    memset(&(v->h), 0, sizeof(struct exprm_state_history)); /* clear expression mode state history*/

    start_tool();                    /* Display the calculator. */
}


/* Calctools' customised math library error-handling routine. */

void
doerr(char *errmes)
{
    if (!v->started) {
        return;
    }

    switch (v->syntax) {
        case npa:
            strncpy(v->display, errmes, MAXLINE - 1);
            v->display[MAXLINE - 1] = '\0';
            set_error_state(TRUE);
            set_display(v->display, FALSE);
            beep();
            break;

        case exprs:
            v->math_error = -MPMATH_ERR;
            break;
    }
}


static int b_buttons_n[B_NOBUTTONS] = {
    5,  6,  7,  13,     /* Bksp  CE  Clr  +/- */
    0,  1,  2,  3,      /*  7    8    9    /  */
    8,  9,  10, 11,     /*  4    5    6    *  */
    16, 17, 18, 19,     /*  1    2    3    -  */
    24, 25, 26, 27,     /*  0    .    =    +  */
};

static void
init_buttons()         /* Setup buttons for "Basic" mode. */
{
    int i;

    for (i = 0; i < B_NOBUTTONS; i++) {
        b_buttons[i] = a_buttons[b_buttons_n[i]];
    }
}


static void
init_text()         /* Setup constant strings. */
{
    STRNCPY(v->con_names[0], _("Kilometer-to-mile conversion factor"),
            MAXLINE - 1);
    STRNCPY(v->con_names[1], _("square root of 2"), MAXLINE - 1);
    STRNCPY(v->con_names[2], _("e"), MAXLINE - 1);
    STRNCPY(v->con_names[3], _("pi"), MAXLINE - 1);
    STRNCPY(v->con_names[4], _("Centimeter-to-inch conversion factor"),
            MAXLINE - 1);
    STRNCPY(v->con_names[5], _("degrees in a radian"), MAXLINE - 1);
    STRNCPY(v->con_names[6], _("2 ^ 20"), MAXLINE - 1);
    STRNCPY(v->con_names[7], _("Gram-to-ounce conversion factor"), MAXLINE - 1);
    STRNCPY(v->con_names[8], 
           _("Kilojoule-to-British-thermal-unit conversion factor"),
            MAXLINE - 1);
    STRNCPY(v->con_names[9], 
           _("Cubic-centimeter-to-cubic-inch conversion factor"), MAXLINE - 1);
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
