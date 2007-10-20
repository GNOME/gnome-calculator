
/*  $Header$
 *
 *  Copyright (c) 1987-2007 Sun Microsystems, Inc. All Rights Reserved.
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
    1.298074214e+33,    /* Binary. */
    2.037035976e+90,    /* Octal. */
    1.000000000e+100,   /* Decimal */
    2.582249878e+120    /* Hexadecimal. */
};

char *calc_res[] = {
    "accuracy", "base", "display", "modetype", "showregisters", "trigtype",
    "showzeroes", "showthousands", "syntax", "xposition", "yposition",
    "register0", "register1", "register2", "register3", "register4",
    "register5", "register6", "register7", "register8", "register9", "bitcalculating"
};

char *mstrs[] = {              /* Mode titles to be added to the titlebar. */
    N_("Basic"), N_("Advanced"), N_("Financial"), 
    N_("Scientific"), N_("Expression")
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

struct button buttons[NKEYS] = {

/* symname
   mods
   value
   func_char
   func
   flags
 */

// FIXME: Move accelerators into gtk.c
{
    N_("0"),
    { 0,     GDK_SHIFT_MASK, 0,        0,             0 },
    { GDK_0, GDK_0,          GDK_KP_0, GDK_KP_Insert, 0 },
    '0',
    do_number,
    number | bin_set | oct_set
},
{
    N_("1"),
    { 0,     GDK_SHIFT_MASK, 0,        0,          0,       0 },
    { GDK_1, GDK_1,          GDK_KP_1, GDK_KP_End, GDK_R13, 0 },
    '1',
    do_number,
    number | bin_set | oct_set
},
{
    N_("2"),
    { 0,     GDK_SHIFT_MASK, 0,        0,           0 }, 
    { GDK_2, GDK_2,          GDK_KP_2, GDK_KP_Down, 0 },
    '2',
    do_number,
    number | oct_set
},    
{     
    N_("3"),
    { 0,     GDK_SHIFT_MASK, 0,        0,                0,       0 },
    { GDK_3, GDK_3,          GDK_KP_3, GDK_KP_Page_Down, GDK_R15, 0 },
    '3',
    do_number,
    number | oct_set
},
{
    N_("4"),
    { 0,     GDK_SHIFT_MASK, 0,        0,           0 },
    { GDK_4, GDK_4,          GDK_KP_4, GDK_KP_Left, 0 },
    '4',
    do_number,
    number | oct_set
},
{
    N_("5"),
    { 0,     GDK_SHIFT_MASK, 0,        0,            0,       0 },
    { GDK_5, GDK_5,          GDK_KP_5, GDK_KP_Begin, GDK_R11, 0 },
    '5',
    do_number,
    number | oct_set
},
{
    N_("6"),
    { 0,     GDK_SHIFT_MASK, 0,        0,            0 },
    { GDK_6, GDK_6,          GDK_KP_6, GDK_KP_Right, 0 },
    '6',
    do_number,
    number | oct_set
},
{
    N_("7"),
    { 0,     GDK_SHIFT_MASK, 0,        0,           0,      0 },
    { GDK_7, GDK_7,          GDK_KP_7, GDK_KP_Home, GDK_R7, 0 },
    '7',
    do_number,
    number | oct_set
},
{
    N_("8"),
    { 0,     GDK_SHIFT_MASK, 0,        0,         0 },
    { GDK_8, GDK_8,          GDK_KP_8, GDK_KP_Up, 0 },
    '8',
    do_number,
    number
},
{
    N_("9"),
    { 0,     GDK_SHIFT_MASK, 0,        0,              0,      0 },
    { GDK_9, GDK_9,          GDK_KP_9, GDK_KP_Page_Up, GDK_R9, 0 },
    '9',
    do_number,
    number
},
{
    N_("A"),
    { 0,     0 },
    { GDK_a, 0 },
    'a',
    do_number,
    number | hex
},
{
    N_("B"),
    { 0,     0 },
    { GDK_b, 0 },
    'b',
    do_number,
    number | hex
},    
{     
    N_("C"),
    { 0,     0 },
    { GDK_c, 0 },
    'c',
    do_number,
    number | hex
},
{
    N_("D"),
    { 0,     0 },
    { GDK_d, 0 },
    'd',
    do_number,
    number | hex
},
{
    N_("E"),
    { 0,     0 },
    { GDK_e, 0 },
    'e',
    do_number,
    number | hex
},
{
    N_("F"),
    { 0,     0 },
    { GDK_f, 0 },
    'f',
    do_number,
    number | hex
},
{    
    N_("."),
    { 0,          0,              0,             0 },
    { GDK_period, GDK_KP_Decimal, GDK_KP_Delete, GDK_KP_Separator, 0 },
    '.',
    do_point,
    number | dpoint
},
{
    N_("="),
    { 0,         0,            0,          GDK_SHIFT_MASK, 0 },
    { GDK_equal, GDK_KP_Enter, GDK_Return, GDK_equal,      0 },
    '=',
    do_calc,
    enter
},
{
    N_("Clr"),
    { 0, 0 },
    { GDK_Delete, 0 },
    '\177',
    do_clear,
    clear
},
{
    N_("Clear entry"),
    { GDK_CONTROL_MASK, 0,          0 },
    { GDK_BackSpace,    GDK_Escape, 0 },
    '\013',
    do_clear_entry,
    clear
},
{
    N_("("),
    { GDK_SHIFT_MASK, 0 },
    { GDK_parenleft, 0 },
    '(',
    do_paren,
    parenthesis
},
{
    N_(")"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_parenright, 0 },
    ')',
    do_paren,
    parenthesis
},
{
    N_("+"),
    { GDK_SHIFT_MASK, 0,        0,          0 },
    { GDK_plus,       GDK_plus, GDK_KP_Add, 0 },
    '+',
    do_calc,
    binop
},
{
    N_("-"),
    { 0,         0,               0,      0 },
    { GDK_minus, GDK_KP_Subtract, GDK_R4, 0 },
    '-',
    do_calc,
    unop | binop
},
{
    N_("*"),
    { GDK_SHIFT_MASK, 0,               0,     0,      0 },
    { GDK_asterisk,   GDK_KP_Multiply, GDK_x, GDK_R6, 0 },
    '*',
    do_calc,
    binop
},
{
    N_("/"),
    { 0,         GDK_SHIFT_MASK, 0,             0,      GDK_SHIFT_MASK, 0 },
    { GDK_slash, GDK_slash,      GDK_KP_Divide, GDK_R5, GDK_slash,      0 },
    '/',
    do_calc,
    binop
},
{
    NULL,
    { 0, 0 },
    { GDK_BackSpace, 0 },
    '\010',
    do_delete,
    bsp
},
{
    N_("Chs"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_C,          0 },
    'C',
    do_immed, 
    neg
},
{
    N_("Int"),
    { 0, 0 },
    { GDK_i, 0 },
    'i',
    do_portion,
    func
},
{
    N_("Frac"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_colon, 0 },
    ':',
    do_portion,
    func  
},
{
    N_("%"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_percent, 0 },
    '%',
    do_percent,  
    immediate
},
{
    N_("^2"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_at, 0 },
    '@',
    do_immed,  
    immediate | postfixop
},
{
    N_("Sqrt"),
    { 0, 0 },   
    { GDK_s, 0 },
    's',
    do_immed, 
    func
},
{
    N_("Recip"),
    { 0, 0 },
    { GDK_r, 0 },
    'r',
    do_immed, 
    inv
},
{
    N_("e^"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_braceleft, 0 },
    '{',
    do_immed, 
    immediate | prefixop
},
{
    N_("10^"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_braceright, 0 },
    '}',
    do_immed, 
    prefixop
},       
{
    N_("^"),
    { GDK_SHIFT_MASK, GDK_SHIFT_MASK,  0 },
    { GDK_caret,      GDK_asciicircum, 0 },
    '^',
    do_calc,
    binop | postfixop
},
{
    N_("!"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_exclam, 0 },
    '!',
    do_immed, 
    immediate | postfixop
},
{
    N_("Rand"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_question, 0 },
    '?',
    do_immed, 
    none
},
{
    N_("Sin"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_K, 0 },
    'K',
    do_trig,
    func
},
{
    N_("Cos"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_J, 0 },
    'J',
    do_trig,
    func
},
{        
    N_("Tan"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_L, 0 },
    'L',
    do_trig,
    func
},
{      
    N_("Ln"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_N, 0 },
    'N',
    do_immed, 
    func
},
{ 
    N_("Log"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_G, 0 },
    'G',
    do_immed, 
    func
},
{
    /* Note to translators: Abs is short for Absolute. */
    N_("Abs"),
    { 0, 0 },
    { GDK_u, 0 },
    'u',
    do_portion,
    func
},
{
    N_("u16"),
    { 0, 0 },        
    { GDK_bracketright, 0 },
    ']',
    do_immed,  
    func
},            
{
    N_("u32"),
    { 0, 0 },
    { GDK_bracketleft, 0 },
    '[',
    do_immed,  
    func
},
{
    N_(" Mod "),
    { GDK_SHIFT_MASK, 0 },
    { GDK_M,          0 },
    'M',
    do_calc,
    binop
},
{
    N_("e"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_E,          0 },
    'E',
    do_expno,
    expnum
},
{
    N_("~"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_asciitilde, 0 },
    '~',
    do_immed, 
    unop | immediate
},
{
    N_(" OR "),
    { GDK_SHIFT_MASK, 0 },
    { GDK_bar, 0 },
    '|',
    do_calc,
    binop
},
{
    N_(" AND "),
    { GDK_SHIFT_MASK, 0 },
    { GDK_ampersand, 0 },
    '&',
    do_calc,
    binop
},       
{
    N_(" XOR "),
    { 0, 0 },   
    { GDK_x, 0 },
    'x',
    do_calc,
    binop
},
{
    N_(" XNOR "),
    { 0, 0 },
    { GDK_n, 0 },
    'n',
    do_calc,
    binop
},
{
    N_("Ctrm"),
    { 0, 0 },
    { GDK_m, 0 },
    'm',
    do_business,
    none
},
{
    N_("Ddb"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_D,          0 },
    'D',
    do_business,
    none
},
{
    N_("Fv"),
    { 0, 0 },
    { GDK_v, 0 },
    'v',
    do_business,
    none
},
{
    N_("Pmt"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_P, 0 },
    'P',
    do_business,
    none
},
{
    N_("Pv"),
    { 0, 0 },
    { GDK_p, 0 },
    'p',
    do_business,
    none
},
{
    N_("Rate"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_T, 0 },
    'T',
    do_business,
    none
},
{
    N_("Sln"),
    { 0, 0 },
    { GDK_l, 0 },
    'l',
    do_business,
    none
},
{ 
    N_("Syd"),
    { 0, 0 },
    { GDK_Y, 0 },
    'Y',
    do_business,
    none
},
{ 
    N_("Term"),
    { 0, 0 },
    { GDK_T, 0 },
    'T',
    do_business,
    none
},
{
    N_("<"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_less, 0 },
    '<',
    do_pending,
    pending
},
{
    N_(">"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_greater, 0 },
    '>',
    do_pending,
    pending
},
{
    N_("Sto"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_S, 0 },
    'S',
    do_pending,
    pending
},
{
    N_("Rcl"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_R, 0 },
    'R',
    do_pending,
    pending
},
{
    N_("Exch"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_X, 0 },
    'X',
    do_pending,
    pending
},
{
    N_("Acc"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_A,          0 },
    'A',
    do_pending,
    pending
},
{
    N_("Con"),
    { GDK_SHIFT_MASK, 0,              0 },
    { GDK_numbersign, GDK_numbersign, 0 },   
    '#',
    do_pending,
    pending
},
{
    N_("Fun"),
    { GDK_SHIFT_MASK, 0 },
    { GDK_F,          0 },
    'F',
    do_pending,
    pending
},
};

void
do_calctool(int argc, char **argv)
{
    char *ptr;
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

    v->radix = get_radix();    /* Locale specific radix string. */
    v->tsep  = get_tsep();     /* Locale specific thousands seperator. */

    init_text();               /* Setup text strings depending upon language. */
    init_vars();               /* Setup default values for variables. */
    load_resources();          /* Get resources from various places. */
    read_resources();          /* Read resources from merged database. */
    get_options(argc, argv);   /* Get command line arguments. */
    read_cfdefs();             /* Read constant/function definitions. */
    make_frames();             /* Create gcalctool window frames. */

    v->current    = copy_button_info(button_for_value(buttons[KEY_CALCULATE].value[0]));
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
