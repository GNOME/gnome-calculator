
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
    N_("_Bin"), N_("_Oct"), N_("_Dec"), N_("He_x")
};

char *base_desc[]  = {         /* Tooltips for each base value. */
    N_("Set numeric base to binary (base 2)"), 
    N_("Set numeric base to octal (base 8)"), 
    N_("Set numeric base to decimal (base 10)"), 
    N_("Set numeric base to hexadecimal (base 16)")
};

char *calc_res[] = {
    "accuracy", "base", "display", "mode", "showregisters", "trigtype",
    "removezeroes"
};

char *dtype_str[] = {          /* Strings for each display mode value. */
    N_("E_ng"), N_("_Fix"), N_("_Sci")
};

char *dtype_desc[] = {         /* Tooltips for each display mode value. */
    N_("Set display type to engineering format"), 
    N_("Set display type to fixed-point format"), 
    N_("Set display type to scientific format")
};

char *mode_str[]  = {          /* Strings for each mode value. */
    N_("BASIC"), N_("FINANCIAL"), N_("SCIENTIFIC")
};

char *mstrs[] = {              /* Mode titles to be added to the titlebar. */
    N_("Basic Mode"), N_("Financial Mode"), N_("Scientific Mode")
};


char *ttype_str[] = {          /* Strings for each trig type value. */
    N_("De_grees"), N_("Gr_adients"), N_("_Radians")
};

char *ttype_desc[] = {         /* Tooltips for each trig type value. */
    N_("Set trigonometric type to degrees"), 
    N_("Set trigonometric type to gradients"), 
    N_("Set trigonometric type to radians")
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
    N_("Numeric 7"),
    CALC_MOD_NONE,
    CALC_KEY_7,
    '7',
    M_NONE,
    do_number
},
{
    N_("8"),
    N_("Numeric 8"),
    CALC_MOD_NONE,
    CALC_KEY_8,
    '8',
    M_NONE,
    do_number
},
{
    N_("9"),
    N_("Numeric 9"),
    CALC_MOD_NONE,
    CALC_KEY_9,
    '9',
    M_NONE,
    do_number
},
{    
    N_("/"),
    N_("Divide"),
    CALC_MOD_NONE,
    CALC_KEY_DIV,
    '/',
    M_NONE,
    do_calc
},
{
    "    ",
    "    ",
    CALC_MOD_NONE,
    CALC_KEY_NONE,
    ' ',
    M_NONE,
    do_none
},
{
    N_("Bsp"),
    N_("Remove rightmost character from displayed value"),
    CALC_MOD_NONE,
    CALC_KEY_BSP,
    '\010',
    M_NONE,
    do_delete
},
{
    N_("CE"),
    N_("Clear displayed value"),
    CALC_MOD_CONTROL_MASK,
    CALC_KEY_CE,
    '\013',
    M_NONE,
    do_clear_entry
},
{
    N_("Clr"),
    N_("Clear displayed value and any partial calculation"),
    CALC_MOD_NONE,
    CALC_KEY_CLR,
    '\177',
    M_NONE,
    do_clear
},

/* Row 2. */
{
    N_("4"),
    N_("Numeric 4"),
    CALC_MOD_NONE,
    CALC_KEY_4,
    '4',
    M_NONE,
    do_number
},
{
    N_("5"),
    N_("Numeric 5"),
    CALC_MOD_NONE,
    CALC_KEY_5,
    '5',
    M_NONE,
    do_number
},
{
    N_("6"),
    N_("Numeric 6"),
    CALC_MOD_NONE,
    CALC_KEY_6,
    '6',
    M_NONE,
    do_number
},
{
    N_("*"),
    N_("Multiply"),
    CALC_MOD_NONE,
    CALC_KEY_MUL,
    '*',
    M_NONE,
    do_calc
},
{
    "    ",
    "    ",
    CALC_MOD_NONE,
    CALC_KEY_NONE,
    ' ',
    M_NONE,
    do_none
},
{
    N_("+/-"),
    N_("Change sign"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_CHS,
    'C',
    M_NONE,
    do_immed
},
{
    N_("Int"),
    N_("Integer portion of displayed value"),
    CALC_MOD_NONE,
    CALC_KEY_INT,
    'i',
    M_NONE,
    do_portion
},
{
    N_("Sto"),
    N_("Store displayed value in memory register"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_STO,
    'S',
    M_STO,
    do_pending
},

/* Row 3. */
{
    N_("1"),
    N_("Numeric 1"),
    CALC_MOD_NONE,
    CALC_KEY_1,
    '1',
    M_NONE,
    do_number
},
{
    N_("2"),
    N_("Numeric 2"),
    CALC_MOD_NONE, 
    CALC_KEY_2,
    '2',
    M_NONE,
    do_number
},    
{     
    N_("3"),
    N_("Numeric 3"),
    CALC_MOD_NONE,
    CALC_KEY_3,
    '3',
    M_NONE,
    do_number
},
{
    N_("-"),
    N_("Subtract"),
    CALC_MOD_NONE,
    CALC_KEY_SUB,
    '-',
    M_NONE,
    do_calc
},
{
    N_("%"),
    N_("Percentage"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_PER,
    '%',
    M_NONE,
    do_calc
},
{
    N_("Sqrt"),
    N_("Square root"),
    CALC_MOD_NONE,   
    CALC_KEY_SQRT,
    's',
    M_NONE,
    do_immed
},
{
    N_("Frac"),
    N_("Fractional portion of displayed value"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_FRAC,
    ':',
    M_NONE,
    do_portion
},
{
    N_("Rcl"),
    N_("Retrieve memory register to display"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_RCL,
    'R',
    M_RCL,
    do_pending
},

/* Row 4. */
{
    N_("0"),
    N_("Numeric 0"),
    CALC_MOD_NONE,
    CALC_KEY_0,
    '0',
    M_NONE,
    do_number
},
{    
    N_("."),
    N_("Numeric point"),
    CALC_MOD_NONE,
    CALC_KEY_PNT,
    '.',
    M_NONE,
    do_point
},
{
    N_("="),
    N_("Calculate result"),
    CALC_MOD_NONE,
    CALC_KEY_EQ,
    '=',
    M_NONE,
    do_calc
},
{
    N_("+"),
    N_("Add"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_ADD,
    '+',
    M_NONE,
    do_calc
},
{
    N_("1/x"),
    N_("Reciprocal"),
    CALC_MOD_NONE,
    CALC_KEY_REC,
    'r',
    M_NONE,
    do_immed
},
{
    N_("x<sup>2</sup>"),
    N_("Square"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_SQR,
    '@',
    M_NONE,
    do_immed
},
{
    N_("Abs"),
    N_("Absolute value"),
    CALC_MOD_NONE,
    CALC_KEY_ABS,
    'u',
    M_NONE,
    do_portion
},
{
    N_("Exch"),
    N_("Exchange displayed value with memory register"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_EXCH,
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
    N_("Compounding term"),
    CALC_MOD_NONE,
    CALC_KEY_CTRM,
    'm',
    M_NONE,
    do_business
},
{
    N_("Ddb"),
    N_("Double-declining depreciation"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_DDB,
    'D',
    M_NONE,
    do_business
},
{
    N_("Fv"),
    N_("Future value"),
    CALC_MOD_NONE,
    CALC_KEY_FV,
    'v',
    M_NONE,
    do_business
},
{
    N_("Pmt"),
    N_("Periodic payment"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_PMT,
    'P',
    M_NONE,
    do_business
},
{
    N_("Pv"),
    N_("Present value"),
    CALC_MOD_NONE,
    CALC_KEY_PV,
    'p',
    M_NONE,
    do_business
},
{
    N_("Rate"),
    N_("Periodic interest rate"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_RATE,
    'T',
    M_NONE,
    do_business
},
{
    N_("Sln"),
    N_("Straight-line depreciation"),
    CALC_MOD_NONE,
    CALC_KEY_SLN,
    'l',
    M_NONE,
    do_business
},
{ 
    N_("Syd"),
    N_("Sum-of-the years'-digits depreciation"),
    CALC_MOD_NONE,
    CALC_KEY_SYD,
    'Y',
    M_NONE,
    do_business
},

/* Row 2. */
{ 
    N_("Term"),
    N_("Payment period"),
    CALC_MOD_NONE,
    CALC_KEY_TERM,
    'T',
    M_NONE,
    do_business
},
{ 
    "    ",
    "    ",
    CALC_MOD_NONE,
    CALC_KEY_NONE,
    ' ',
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    CALC_MOD_NONE,
    CALC_KEY_NONE,
    ' ',
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    CALC_MOD_NONE,
    CALC_KEY_NONE,
    ' ',
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    CALC_MOD_NONE,
    CALC_KEY_NONE,
    ' ',
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    CALC_MOD_NONE,
    CALC_KEY_NONE,
    ' ',
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    CALC_MOD_NONE,
    CALC_KEY_NONE,
    ' ',
    M_NONE,
    do_none
},
{
    "    ",
    "    ",
    CALC_MOD_NONE,
    CALC_KEY_NONE,
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
    N_("Shift displayed value 1-15 places to the left"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_LSFT,
    '<',
    M_LSHF,
    do_pending
},
{
    N_(">"),
    N_("Shift displayed value 1-15 places to the right"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_RSFT,
    '>',
    M_RSHF,   
    do_pending
},            
{             
    N_("&amp;16"),
    N_("16-bit unsigned integer value of display"),
    CALC_MOD_NONE,        
    CALC_KEY_16,
    ']',
    M_NONE,   
    do_immed  
},            
{             
    N_("&amp;32"),
    N_("32-bit unsigned integer value of display"),
    CALC_MOD_NONE,
    CALC_KEY_32,
    '[',
    M_NONE,
    do_immed
},
{
    "    ",
    "    ",
    CALC_MOD_NONE,
    CALC_KEY_NONE,
    ' ',
    M_NONE,
    do_none
},
{
    N_("("),
    N_("Start group of calculations"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_LPAR,
    '(',
    M_NONE,
    do_paren
},
{
    N_(")"),
    N_("End group of calculations"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_RPAR,
    ')',
    M_NONE,
    do_paren
},
{
    N_("Acc"),
    N_("Set accuracy from 0 to 9 numeric places"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_ACC,
    'A',
    M_ACC,
    do_pending
},

/* Row 2. */
{
    N_("Con"),
    N_("Constants"),
    CALC_MOD_SHIFT_MASK,   
    CALC_KEY_CON,   
    '#',
    M_CON,            
    do_pending
},
{
    N_("Fun"),
    N_("User-defined functions"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_FUN,
    'F',
    M_FUN,
    do_pending
},
{
    N_("Exp"),
    N_("Enter an exponential number"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_EXP,
    'E',
    M_NONE,
    do_expno
},
{
    N_("e<sup>x</sup>"),
    N_("e to the power of displayed value"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_ETOX,
    '{',
    M_NONE,
    do_immed
},
{
    N_("10<sup>x</sup>"),
    N_("10 to the power of displayed value"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_TTOX,
    '}',
    M_NONE,
    do_immed
},       
{        
    N_("y<sup>x</sup>"),
    N_("y to the power of displayed value"),
    CALC_MOD_NONE,   
    CALC_KEY_YTOX,
    'y',
    M_NONE,
    do_calc
},       
{        
    N_("x!"),
    N_("Factorial of displayed value"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_FACT,
    '!',
    M_NONE,
    do_immed
},
{
    N_("Rand"),
    N_("Random number in the range 0.0 to 1.0"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_RAND,
    '?',
    M_NONE,
    do_immed
},

/* Row 3. */
{
    N_("D"),
    N_("Hexadecimal digit D"),
    CALC_MOD_NONE,
    CALC_KEY_D,
    'd',
    M_NONE,
    do_number
},
{
    N_("E"),
    N_("Hexadecimal digit E"),
    CALC_MOD_NONE,
    CALC_KEY_E,
    'e',
    M_NONE,
    do_number
},
{
    N_("F"),
    N_("Hexadecimal digit F"),
    CALC_MOD_NONE,
    CALC_KEY_F,
    'f',
    M_NONE,
    do_number
},
{
    N_("Cos"),
    N_("Cosine"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_COS,
    'J',
    M_NONE,
    do_trig
},
{
    N_("Sin"),
    N_("Sine"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_SIN,
    'K',
    M_NONE,
    do_trig
},
{        
    N_("Tan"),
    N_("Tangent"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_TAN,
    'L',
    M_NONE,
    do_trig
},     
{      
    N_("Ln"),
    N_("Natural log"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_LN,
    'N',
    M_NONE,
    do_immed
},
{ 
    N_("Log"),
    N_("Base 10 log"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_LOG,
    'G',
    M_NONE,
    do_immed
},

/* Row 4. */
{
    N_("A"),
    N_("Hexadecimal digit A"),
    CALC_MOD_NONE,
    CALC_KEY_A,
    'a',
    M_NONE,
    do_number
},
{
    N_("B"),
    N_("Hexadecimal digit B"),
    CALC_MOD_NONE,
    CALC_KEY_B,
    'b',
    M_NONE,
    do_number
},    
{     
    N_("C"),
    N_("Hexadecimal digit C"),
    CALC_MOD_NONE,
    CALC_KEY_C,
    'c',
    M_NONE,
    do_number
},
{
    N_("Or"),
    N_("Logical OR"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_OR,
    '|',
    M_NONE,
    do_calc
},
{
    N_("And"),
    N_("Logical AND"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_AND,
    '&',
    M_NONE,
    do_calc
},       
{        
    N_("Not"),
    N_("Logical NOT"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_NOT,
    '~',
    M_NONE,
    do_immed
},
{
    N_("Xor"),
    N_("Logical XOR"),
    CALC_MOD_SHIFT_MASK,
    CALC_KEY_XOR,
    '^',
    M_NONE,
    do_calc
},
{
    N_("Xnor"),
    N_("Logical XNOR"),
    CALC_MOD_NONE,
    CALC_KEY_XNOR,
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
    init_key_types();          /* Assign toolkit specific key values. */
    load_resources();          /* Get resources from various places. */
    read_resources();          /* Read resources from merged database. */
    get_options(argc, argv);   /* Get command line arguments. */
    read_cfdefs();             /* Read constant/function definitions. */
    make_frames();             /* Create gcalctool window frames. */

    v->current    = copy_button_info(button_for_value(KEY_EQ.value));
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
        if (!v->iconic) {
            win_display(FCP_REG, TRUE);
        }
    }

    SPRINTF(title, "%s [%s]", v->tool_label, _(mstrs[(int) v->modetype]));
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
    STRCPY(v->con_names[0], _("kilometers per hour or miles per hour"));
    STRCPY(v->con_names[1], _("square root of 2"));
    STRCPY(v->con_names[2], _("e"));
    STRCPY(v->con_names[3], _("pi"));
    STRCPY(v->con_names[4], _("centimeters or inches"));
    STRCPY(v->con_names[5], _("degrees in a radian"));
    STRCPY(v->con_names[6], _("2 ^ 20"));
    STRCPY(v->con_names[7], _("grams or ounces"));
    STRCPY(v->con_names[8], _("kilojoules or British thermal units"));
    STRCPY(v->con_names[9], _("cubic centimeters or cubic inches"));
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
