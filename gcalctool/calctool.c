
/*  $Header$
 *
 *  Copyright (c) 1987-2002, Sun Microsystems, Inc.  All Rights Reserved.
 *  Sun considers its source code as an unpublished, proprietary
 *  trade secret, and it is available only under strict license
 *  provisions.  This copyright notice is placed here only to protect
 *  Sun in the event the source is deemed a published work.  Dissassembly,
 *  decompilation, or other means of reducing the object code to human
 *  readable form is prohibited by the license agreement under which
 *  this code is provided to the user or company in possession of this
 *  copy.
 *
 *  RESTRICTED RIGHTS LEGEND: Use, duplication, or disclosure by the
 *  Government is subject to restrictions as set forth in subparagraph
 *  (c)(1)(ii) of the Rights in Technical Data and Computer Software
 *  clause at DFARS 52.227-7013 and in similar clauses in the FAR and
 *  NASA FAR Supplement.
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
    N_("BIN"), N_("OCT"), N_("DEC"), N_("HEX")
};

char *calc_res[] = {
    "accuracy",      "base",        "display",  "mode", "showhelp",
    "showregisters", "righthanded", "trigtype", "beep"
};

char *dtype_str[] = {          /* Strings for each display mode value. */
    N_("ENG"), N_("FIX"), N_("SCI")
};

char *mode_str[]  = {          /* Strings for each mode value. */
    N_("BASIC"), N_("FINANCIAL"), N_("LOGICAL"), N_("SCIENTIFIC")
};

char *mstrs[] = {              /* Mode titles for the popup panel. */
    N_("Basic Mode"),   N_("Financial Mode"),
    N_("Logical Mode"), N_("Scientific Mode")
};


char *ttype_str[] = {          /* Strings for each trig type value. */
    N_("DEG"), N_("GRAD"), N_("RAD")
};

char digits[] = "0123456789ABCDEF";
int basevals[4] = { 2, 8, 10, 16 };

static int left_pos[BCOLS]  = {   /* "Left-handed" positions. */
    7, 6, 4, 5, 0, 1, 2, 3
};
static int right_pos[BCOLS] = {   /* "Right-handed" positions. */
    0, 1, 2, 3, 4, 5, 6, 7 
};
int cur_pos[BCOLS] = {            /* Current positions - initially "right". */
    0, 1, 2, 3, 4, 5, 6, 7 
};


/* Various string values read/written as X resources. */

char *Rbstr[MAXBASES]     = { "BIN", "OCT", "DEC", "HEX" };
char *Rdstr[MAXDISPMODES] = { "ENG", "FIX", "SCI" };
char *Rmstr[MAXMODES]     = { "BASIC", "FINANCIAL", "LOGICAL", "SCIENTIFIC" };
char *Rtstr[MAXTRIGMODES] = { "DEG", "GRAD", "RAD" };

/* Valid keys when an error condition has occured. */
/*                            MEM  KEYS clr         QUIT REDRAW */
int validkeys[MAXVKEYS]  = { 'm', 'k',  GDK_Delete, 'q', '\f' };

Vars v;            /* Calctool variables and options. */

/*  This table shows the keyboard values that are currently being used:
 *
 *           |  a b c d e f g h i j k l m n o p q r s t u v w x y z
 *-----------+-----------------------------------------------------
 *  Control: |  a   c d   f     i     l m         r s t u       y
 *  Lower:   |  a b c d e f   h i   k   m n   p q r s t   v   x y
 *  Upper:   |  A B C D E F G           M N   P Q R S T       X
 *  Numeric: |  0 1 2 3 4 5 6 7 8 9
 *  Other:   |  @ . + - * / = % ( ) # < > [ ] { } | & ~ ^ ? !
 *           |  BackSpace Delete Return
 *-----------+-----------------------------------------------------
 */

struct button buttons[NOBUTTONS] = {  /* Calculator button values. */

/* str
   hstr
   mods
   value
   opdisp
   menutype
   func
 */

/* Row 1. */
{ 
    N_("Disp"),
    N_(
      "Disp\n"
      " \n"
      "Changes the type of numeric display.\n"
      " \n"
      "Keyboard equivalent:   D\n"
      " \n"
      "Initially the numeric display is fixed-point\n"
      "notation to an accuracy of two decimal places.\n"
      "You can change this to engineering or scientific\n"
      "display.\n"
      " \n"
      "Click on the Disp button to show the available settings.\n"
      "Choose a value from the menu to change to a new\n"
      "numeric display.\n"
      " \n"
      "The calculator display (plus the memory registers,\n"
      "if they are showing) will be redrawn using the\n"
      "new notation.\n"
      " \n"
      "Alternatively, you can use the keyboard to set\n"
      "the numeric display: Type D, followed by\n"
      "e, f, or s to indicate the new display setting:\n"
      " \n"
      "e   engineering\n"
      "f   fixed-point\n"
      "s   scientific"
    ),
    GDK_SHIFT_MASK,
    GDK_D,
    OP_SET,
    M_NUM,
    do_pending
},
{    
    N_("Base"),
    N_(
      "Base\n"
      " \n"
      "Changes numeric base.\n"
      " \n"
      "Keyboard equivalent:   B\n"
      " \n"
      "Initially the numeric base is decimal. You can\n"
      "change the base to binary, octal, or hexadecimal.\n"
      " \n"
      "Click on the Base button to show the available settings.\n"
      "Choose a value from the menu to change to a new\n"
      "numeric base.\n"
      " \n"
      "Depending on that base, various keys on the\n"
      "calculator will be dimmed to show that they\n"
      "are disabled.\n"
      " \n"
      "Alternatively, you can use the keyboard to set\n"
      "the numeric base: Type B, followed by\n"
      "b, o, d, or h to indicate the new base setting:\n"
      " \n"
      "b   binary\n"
      "o   octal\n"
      "d   decimal\n"
      "h   hexadecimal"
    ),
    GDK_SHIFT_MASK,
    GDK_B,
    OP_SET,
    M_BASE,
    do_pending
},
{
    N_("Int"),
    N_(
      "Int\n"
      "\n"
      "Integer portion\n"
      "\n"
      "Keyboard equivalent:   Control-i\n"
      "\n"
      "Returns the integer portion of the current\n"
      "displayed value."
    ),
    GDK_CONTROL_MASK,
    GDK_i,
    OP_CLEAR,
    M_NONE,
    do_portion
},
{
    N_("Frac"),
    N_(
      "Frac\n"
      " \n"
      "Fractional portion\n"
      "\n"
      "Keyboard equivalent:   Control-f\n"
      " \n"
      "Returns the fractional portion of the current\n"
      "displayed value."
    ),
    GDK_CONTROL_MASK,
    GDK_f,
    OP_CLEAR,
    M_NONE,
    do_portion
},
{
    N_("D"),
    N_(
      "Hex D (decimal 13)\n"
      "\n"
      "Keyboard equivalent:   d\n"
      "\n"
      "Enters the hexadecimal value d in the display.\n"
      "\n"
      "Available only when the current base is\n"
      "hexadecimal."
    ),
    0,
    GDK_d,
    OP_NOP,
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
      "Available only when the current base is\n"
      "hexadecimal."
    ),
    0,
    GDK_e,
    OP_NOP,
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
      "Available only when the current base is\n"
      "hexadecimal."
    ),
    0,
    GDK_f,
    OP_NOP,
    M_NONE,
    do_number
},
{    
    N_("Clr"),
    N_(
      "Clr\n"
      "\n"
      "Clears the display.\n"
      "\n"
      "Keyboard equivalent:   Delete\n"
      "\n"
      "This clears the value currently displayed."
    ),
    0,
    GDK_Delete,
    OP_CLEAR,
    M_NONE,
    do_clear
},

/* Row 2. */
{
    N_("Mode"),
    N_(
      "Mode\n"
      " \n"
      "Changes the calculator mode.\n"
      " \n"
      "Keyboard equivalent:   M\n"
      " \n"
      "By default, the initial calculator mode is Basic.\n"
      " \n"
      "Click on the Mode button to display and choose the\n"
      "other available modes:\n"
      " \n"
      "- Financial\n"
      "- Logical\n"
      "- Scientific\n"
      " \n"
      " \n"
      "If you choose one of these modes, a window with\n"
      "extra buttons is displayed.\n"
      " \n"
      "Alternatively, you can set the calculator mode\n"
      "using the keyboard. Type M, followed by n, s, or b\n"
      "to indicate the new calculator mode.\n"
      " \n"
      "b   Basic\n"
      "f   Financial\n"
      "l   Logical\n"
      "s   Scientific"
    ),
    GDK_SHIFT_MASK,
    GDK_M,
    OP_SET,
    M_MODE,
    do_pending
},
{
    N_("Keys"),
    N_(
      "Keys\n"
      " \n"
      "Toggles button labels.\n"
      " \n"
      "Keyboard equivalent:   k\n"
      " \n"
      "Toggles the labels on the calctool keys,\n"
      "alternating between the mouse and keyboard\n"
      "equivalents."
    ),
    0,
    GDK_k,
    OP_CLEAR,
    M_NONE,
    do_keys
},
{
    N_("Abs"),
    N_(
      "Abs\n"
      " \n"
      "Absolute value.\n"
      " \n"
      "Keyboard equivalent:   Control-u\n"
      " \n"
      "Returns the absolute value of the current\n"
      "displayed value."
    ),
    GDK_CONTROL_MASK,
    GDK_u,
    OP_CLEAR,
    M_NONE,
    do_portion
},
{
    N_("+/-"),
    N_(
      "+/-\n"
      " \n"
      "Change sign.\n"
      " \n"
      "Keyboard equivalent:   C\n"
      "\n"
      "Changes the arithmetic sign of the current\n"
      "displayed value or the exponent being entered."
    ),
    GDK_SHIFT_MASK,
    GDK_C,
    OP_CLEAR,
    M_NONE,
    do_immed
},
{
    N_("A"),
    N_(
      "Hex A (decimal 10)\n"
      " \n"
      "Keyboard equivalent:   a\n"
      "\n"
      "Enters the hexadecimal value a in the display.\n"
      " \n"
      "Available only when the current base is\n"
      "hexadecimal."
    ),
    0,
    GDK_a,
    OP_NOP,
    M_NONE,
    do_number
},
{
    N_("B"),
    N_(
      "Hex B (decimal 11)\n"
      " \n"
      "Keyboard equivalent:   b\n"
      " \n"
      "Enters the hexadecimal value b in the display.\n"
      " \n"
      "Available only when the current base is\n"
      "hexadecimal."
    ),
    0,
    GDK_b,
    OP_NOP,
    M_NONE,
    do_number
},
{
    N_("C"),
    N_(
      "Hex C (decimal 12)\n"
      " \n"
      "Keyboard equivalent:   c\n"
      " \n"
      "Enters the hexadecimal value c in the display.\n"
      " \n"
      "Available only when the current base is\n"
      "hexadecimal."
    ),
    0,
    GDK_c,
    OP_NOP,
    M_NONE,
    do_number
},
{
    N_("Bsp"),
    N_(
      "Bsp\n"
      " \n"
      "Erases characters one at a time.\n"
      " \n"
      "Keyboard equivalent:   Back Space\n"
      " \n"
      "This removes the right-most character of the\n"
      "displayed value and recalculates the value of\n"
      "the display.\n"
      " \n"
      "Internal accuracy is lost with this operation."
    ),
    0,
    GDK_BackSpace,
    OP_NOP,
    M_NONE,
    do_delete
},

/* Row 3. */
{
    N_("Mem..."),
    N_(
      "Mem\n"
      "\n"
      "Displays a memory register window.\n"
      "\n"
      "Keyboard equivalent:   m\n"
      "\n"
      "Click on the Mem button to display a window showing\n"
      "the values of the ten memory registers in the\n"
      "current base, to the current accuracy."
    ),
    0,
    GDK_m,
    OP_NOP,
    M_NONE,
    do_memory
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
      "Sets the accuracy of both the display and the\n"
      "memory registers.\n"
      "\n"
      "Click on the Acc button to display a menu of levels\n"
      "of accuracy. Choose a level from the menu to set\n"
      "the precision for the display and registers."
    ),
    GDK_SHIFT_MASK,
    GDK_A,
    OP_SET,
    M_ACC,
    do_pending
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
      "Returns the value 1 divided by the current\n"
      "displayed value."
    ),
    0,
    GDK_r,
    OP_CLEAR,
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
    OP_CLEAR,
    M_NONE,
    do_immed
},
{
    N_("7"),
    N_(
      "Numeric 7\n"
      "\n"
      "Keyboard equivalent:   7\n"
      " \n"
      "This key is inactive in binary base."
    ),
    0,
    GDK_7,
    OP_NOP,
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
    OP_NOP,
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
    OP_NOP,
    M_NONE,
    do_number
},
{
    N_("X"),
    N_(
      "X\n"
      "\n" 
      "Multiplication\n"
      "\n"
      "Keyboard equivalent:   x or *\n"
      "\n"
      "This key takes the last number entered and\n"
      "multiplies it by the next number entered."
    ),
    0,
    GDK_x,
    OP_SET,
    M_NONE,
    do_calc
},

/* Row 4. */
{
    N_("Fun"),
    N_(
      "Fun\n"
      "\n"
      "Menu of defined functions\n"
      "\n"
      "Keyboard equivalent:   F\n"
      "\n"
      "Click on the Fun button to display a menu of\n"
      "functions you have defined. Choose a function from\n"
      "the menu to execute it. The result will show in\n"
      "the display.\n"
      "\n"
      "To add new functions or alter existing ones,\n"
      "choose \"Enter Function\" from the menu. This\n"
      "displays a window in which you type the number,\n"
      "description, and value of the function.\n"
      "\n"
      "Click SELECT on \"New Function\" to store the new\n"
      "function in the ~/.gcalctoolcf file.\n"
      "\n"
      "For the function value, type the keyboard strokes\n"
      "that calculate your function. For example, to\n"
      "calculate sine(90), type the Value:\n"
      "\n"
      "        90 \\s\n"
      "\n"
      "When you type the function value, use \\ to\n"
      "represent the Control key even though KEYS shows\n"
      "the keyboard equivalent of the Control key as\n"
      "the character ^ .\n"
      "\n"
      "When you know the number of the function you\n"
      "want, you can click SELECT on Fun, then on the\n"
      "function's number."
    ),
    GDK_SHIFT_MASK,
    GDK_F,
    OP_SET,
    M_FUN,
    do_pending
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
      "Click on the Con button to show the available constants.\n"
      "Choose a constant from the menu to enter its value\n"
      "in the display.\n"
      "\n"
      "Alternatively, if you know the number of a\n"
      "constant, you can click SELECT on Con, then\n"
      "on a digit from 0 - 9 to indicate the constant\n"
      "you want to enter.\n"
      "\n"
      "To add new constants or alter existing ones,\n"
      "choose \"Enter Constant\" from the menu. This\n"
      "displays a window in which you type the number,\n"
      "description, and value of the constant.\n"
      "\n"
      "Click SELECT on \"Enter Constant\" to store the new\n"
      "constant in the ~/.gcalctoolcf file."
    ),
    GDK_SHIFT_MASK,
    GDK_numbersign,
    OP_SET,
    M_CON,
    do_pending
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
      "Takes the last number entered and calculates a\n"
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
    OP_SET,
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
      "Calculates the square root of the current\n"
      "displayed value."
    ),
    0,
    GDK_s,
    OP_CLEAR,
    M_NONE,
    do_immed
},
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
    OP_NOP,
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
    OP_NOP,
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
    OP_NOP,
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
      "This key takes the last number entered and\n"
      "divides it by the next number entered."
    ),
    0,
    GDK_slash,
    OP_SET,
    M_NONE,
    do_calc
},

/* Row 5. */
{
    N_("Sto"),
    N_(
      "Sto\n"
      "\n"
      "Stores a value in a memory register.\n"
      "\n"
      "Keyboard equivalent:   S\n"
      "\n"
      "Click on the Sto button to display a menu of all registers.\n"
      "Choose a register from the menu to store the\n"
      "displayed value.\n"
      "\n"
      "Alternately, click SELECT on Sto, then on a digit\n"
      "in the range 0 to 9 to indicate the memory\n"
      "register in which to store the displayed value."
    ), 
    GDK_SHIFT_MASK,
    GDK_S,
    OP_SET,
    M_STO,
    do_pending
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
      "Click on the Rcl button to display a menu of available\n"
      "registers. Choose a register from the menu to\n"
      "retrieve its value.\n"
      "\n"
      "When you know the number of the register you\n"
      "want, you can click SELECT on Rcl, then on the\n"
      "register's number (a digit from 0 to 9)."
    ),
    GDK_SHIFT_MASK,
    GDK_R,
    OP_SET,
    M_RCL,
    do_pending
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
      "Allows you to group together a set of\n"
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
    OP_SET,
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
      "Allows you to group together a set of\n"
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
    OP_SET,
    M_NONE,
    do_paren
},
{
    N_("1"),
    N_(
      "Numeric 1\n"
      "\n"
      "Keyboard equivalent:   1"
    ),
    0,
    GDK_1,
    OP_NOP,
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
    OP_NOP,
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
    OP_NOP,
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
      "This operation takes the last number entered and\n"
      "subtracts from it the next number entered."
    ),
    0,
    GDK_minus,
    OP_SET,
    M_NONE,
    do_calc
},

/* Row 6. */
{
    N_("Quit"),
    N_( 
      "Quit\n"
      "\n"
      "Quits the calculator.\n"
      "\n"
      "Keyboard equivalent:   q or Q\n"
      "\n"
      "Quits the calculator without asking for your\n"
      "confirmation."
    ),
    0,
    GDK_q,
    OP_CLEAR,
    M_NONE,
    do_frame
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
      "Click on the Exch button to display the menu of available\n"
      "registers. You can exchange the contents of a\n"
      "register with the current displayed value.\n"
      "\n"
      "Alternatively, click SELECT on Exch, then on a\n"
      "digit from 0 - 9 to indicate which register you\n"
      "want to exchange with the current display.\n"
      "\n"
      "Until you store a value in a register, it has the\n"
      "value 0.00. You can also fill a register by\n"
      "editing the ~/.gcalctoolcf file."
    ),
    GDK_SHIFT_MASK,
    GDK_X,
    OP_SET,
    M_EXCH,
    do_pending
},
{
    N_("Exp"),
    N_(
      "Exp\n"
      "\n"
      "Enters an exponential number.\n"
      "\n"
      "Keyboard equivalent:   E\n"
      "\n"
      "Starts exponential input. Any numbers typed from\n"
      "now on are the exponent. If you haven't entered a\n"
      "mantissa, the calculator uses a mantissa of 1.0."
    ),
    GDK_SHIFT_MASK,
    GDK_E,
    OP_SET,
    M_NONE,
    do_expno
},
{
    N_("Asc..."),
    N_(
      "Asc\n"
      "\n"
      "ASCII value\n"
      "\n"
      "Keyboard equivalent:   Control-a\n"
      "\n"
      "Displays a window in which you type a character\n"
      "whose ASCII value you want. Enter the character\n"
      "and click SELECT on the ASCII button in the window.\n"
      "\n"
      "The calculator displays the ASCII value in the\n"
      "current number base. (You can change the base.)"
    ),
    GDK_CONTROL_MASK,
    GDK_a,
    OP_CLEAR,
    M_NONE,
    do_ascii
},
{
    N_("0"),
    N_(
      "Numeric 0\n"
      "\n"
      "Keyboard equivalent:   0\n"
    ),
    0,
    GDK_0,
    OP_NOP,
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
    OP_NOP,
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
      "Displays the result of the current calculation in\n"
      "the current base."
    ),
    0,
    GDK_equal,
    OP_CLEAR,
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
      "Takes the last number entered and adds it to the\n"
      "next number entered."
    ),
    GDK_SHIFT_MASK,
    GDK_plus,
    OP_SET,
    M_NONE,
    do_calc
},
};

struct button mode_buttons[(MAXMODES-1) * MODEKEYS] = {

/* str
   hstr    
   mods              
   value           
   opdisp   
   menutype 
   func
*/

/* Financial. */
{ 
    N_("Ctrm"),
    N_(
      "Ctrm\n"
      "\n"
      "Compounding term\n"
      "\n"
      "Keyboard equivalent:   Control-m\n"
      "\n"
      "Computes the number of compounding periods it\n"
      "will take an investment of present value pv to\n"
      "grow to a future value of fv, earning a fixed\n"
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
      "You have just deposited $8,000 in an account\n"
      "that pays an annual interest rate of 9%,\n"
      "compounded monthly. You want to determine how\n"
      "long it will take to double you investment.\n"
      "\n"
      "   Register 0 - 0.0075 (interest rate = 9% / 12)\n"
      "   Register 1 - 16000  (future value).\n"
      "   Register 2 - 8000   (present value).\n"
      "\n"
      "Pressing SELECT on Ctrm returns 92.77, which\n"
      "tells you that it would take 92.77 months, or\n"
      "almost eight years, to double your $8,000."
    ),
    GDK_CONTROL_MASK,
    GDK_m,
    OP_CLEAR,
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
      "Keyboard equivalent:   Control-d\n"
      "\n"
      "Computes the depreciation allowance on an asset\n"
      "for a specified period of time, using the\n"
      "double-declining balance method.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - cost    (amount paid for asset)\n"
      "   Register 1 - salvage (value of asset at end\n"
      "                         of its life)\n"
      "   Register 2 - life    (useful life of the asset)\n"
      "   Register 3 - period  (time period for\n"
      "                         depreciation allowance)\n"
      "\n"
      "Example:\n"
      "\n"
      "You have just purchased an office machine for\n"
      "$8,000. The useful life of this machine is\n"
      "six years. The salvage value after six years\n"
      "is $900.\n"
      "\n"
      "To compute the depreciate expense for\n"
      "the fourth year, using the double-declining\n"
      "balance method.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 8000     (amount paid for asset)\n"
      "   Register 1 - 900      (value of asset at end\n"
      "                          of its life)\n"
      "   Register 2 - 6        (useful life of the\n"
      "                          asset)\n"
      "   Register 3 - 4        (time period for\n"
      "                          depreciation allowance)\n"
      "\n"
      "Pressing SELECT on Ddb returns 790.12, which\n"
      "tells you that the depreciation expense for the\n"
      "fourth year will be $790.12."
    ),
    GDK_CONTROL_MASK, 
    GDK_d,
    OP_CLEAR, 
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
      "This calculation determines the future value of\n"
      "an investment. It computes the future value based\n"
      "on a series of equal payments, each of amount pmt,\n"
      "earning periodic interest rate int, over the\n"
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
      "You plan to deposit $4,000 each year for the next\n"
      "20 years into a bank account. The account is\n"
      "paying 8% interest, compounded annually.\n"
      "Interest is paid on the last day of each year.\n"
      "\n"
      "You want to compute the value of your account\n"
      "in 20 years. You make each year's contribution\n"
      "on the last day of the year.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 4000 (periodic payment)\n"
      "   Register 1 - 0.08 (periodic interest rate\n"
      "                      is 8%)\n"
      "   Register 2 - 20   (number of periods)\n"
      "\n"
      "Clicking SELECT on Fv returns 183047.86, the value\n"
      "of your account in dollars at the end of 20 years."
    ),
    0,
    GDK_v,
    OP_CLEAR,
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
      "Computes the amount of the periodic payment\n"
      "of a loan. Most installment loans are computed\n"
      "like ordinary annuities, in that payments are\n"
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
      "You are considering taking out a $120,000 mortgage\n"
      "for 30 years at an annual interest rate of 11.0%.\n"
      "You want to determine your monthly repayment.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 120000  (principal).\n"
      "   Register 1 - 0.00916 (periodic interest rate\n"
      "                         is 11.0% / 12)\n"
      "   Register 2 - 360     (term - 30 x 12)\n"
      "\n"
      "Clicking SELECT on Pmt returns 1142.06, the value\n"
      "in dollars of your monthly repayment."
    ),
    GDK_SHIFT_MASK, 
    GDK_P,
    OP_CLEAR, 
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
      "Determines the present value of an investment. It\n"
      "computes the present value based on a series of\n"
      "equal payments, each of amount pmt, discounted at\n"
      "periodic interest rate int, over the number of\n"
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
      "You have just won a million dollars. The prize\n"
      "is awarded in 20 annual payments of $50,000 each\n"
      "(a total of $1,000,000 over 20 years). Annual\n"
      "payments are received at the end of each year.\n"
      "\n"
      "You are given the option of receiving a single\n"
      "lump-sum payment of $400,000 instead of the\n"
      "million dollars annuity. You want to find out\n"
      "which option is worth more in today's dollars.\n"
      "\n"
      "If you were to accept the annual payments of\n"
      "$50,000, you assume that you would invest the\n"
      "money at a rate of 9%, compounded annually.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 50000 (periodic payment).\n"
      "   Register 1 - 0.09  (periodic interest rate\n"
      "                       is 9%)\n"
      "   Register 2 - 20    (term)\n"
      "\n"
      "Clicking SELECT on Pv returns a value of\n"
      "456427.28, which tells you that the $1,000,000\n"
      "paid over 20 years is worth $456,427.28 in\n"
      "present dollars.\n"
      "\n"
      "Based on your assumptions, the lump-sum\n"
      "payment of $400,000 is worth less than the\n"
      "million-dollar ordinary annuity, in present\n"
      "dollars (before taxes)."
    ),
    0,
    GDK_p, 
    OP_CLEAR,
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
      "Keyboard equivalent:   Control-r\n"
      "\n"
      "Returns the periodic interest necessary for a\n"
      "present value of pv to grow to a future value of\n"
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
      "You have invested $20,000 in a bond. The bond\n"
      "matures in five years, and has a maturity value\n"
      "of $30,000. Interest is compounded monthly. You\n"
      "want to determine the periodic interest rate for\n"
      "this investment.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 30000 (future value)\n"
      "   Register 1 - 20000 (present value)\n"
      "   Register 2 - 60    (term - 5 x 12)\n"
      "\n"
      "Clicking SELECT on Rate returns .00678, which\n"
      "tells you that the periodic (monthly) interest\n"
      "rate is 0.678%, under 1% per month.\n"
      "\n"
      "To determine the annual rate, multiply the above\n"
      "formula by 12, which yields a result of 8.14%."
    ),
    GDK_CONTROL_MASK,
    GDK_r,
    OP_CLEAR,
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
      "Keyboard equivalent:   Control-l\n"
      "\n"
      "Computes the straight-line depreciation of an\n"
      "asset for one period.\n"
      "\n"
      "The straight-line method of depreciation divides\n"
      "the depreciable cost (cost - salvage) evenly over\n"
      "the useful life of an asset. The useful life is\n"
      "the number of periods (typically years) over\n"
      "which an asset is depreciated.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - cost     (cost of the asset)\n"
      "   Register 1 - salvage  (salvage value of the\n"
      "                          asset)\n"
      "   Register 2 - life     (useful life of the\n"
      "                          asset)\n"
      "\n"
      "Example:\n"
      "\n"
      "You have purchased an office machine for $8,000.\n"
      "The useful life of this machine is six years,\n"
      "and the salvage value in eight years will be\n"
      "$900. You want to compute yearly depreciation\n"
      "expense, using the straight-line method.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 8000     (cost of the asset)\n"
      "   Register 1 - 900      (salvage value of the\n"
      "                          asset)\n"
      "   Register 2 - 6        (useful life of the\n"
      "                          asset)\n"
      "\n"
      "Clicking SELECT on Sln returns 1183.33, the yearly\n"
      "dollar depreciation allowance."
    ),
    GDK_CONTROL_MASK,
    GDK_l,
    OP_CLEAR,
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
      "Keyboard equivalent:   Control-y\n"
      "\n"
      "Returns the sum-of-the-years'-digits depreciation\n"
      "for a specified period.\n"
      "\n"
      "The sum-of-the-years'-digits method of\n"
      "depreciation accelerates the rate of depreciation,\n"
      "so that more depreciation expense occurs in\n"
      "earlier periods than in later ones.\n"
      "\n"
      "The depreciable cost is the actual cost minus\n"
      "salvage value. The useful life is the number of\n"
      "periods (typically years) over which an asset is\n"
      "depreciated.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - cost     (cost of the asset)\n"
      "   Register 1 - salvage  (salvage value of\n"
      "                          the asset)\n"
      "   Register 2 - life     (useful life of the\n"
      "                          asset)\n"
      "   Register 3 - period   (period for which\n"
      "                         depreciation is computed)\n"
      "\n"
      "Example:\n"
      "\n"
      "You have just purchased an office machine for\n"
      "$8,000. The useful life of this machine is six\n"
      "years, and the salvage value after eight years\n"
      "will be $900.\n"
      "\n"
      "You want to compute the depreciation expense for\n"
      "the fourth year, using the sum-of-the-years'-\n"
      "digits method.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 8000    (cost of the asset)\n"
      "   Register 1 - 900     (salvage value of\n"
      "                         the asset)\n"
      "   Register 2 - 6       (useful life of the\n"
      "                         asset)\n"
      "   Register 3 - 4       (period for which\n"
      "                         depreciation is computed)\n"
      "\n"
      "Clicking SELECT on Syd returns 1014.29, the dollar\n"
      "depreciation allowance for the fourth year."
    ),
    GDK_CONTROL_MASK,
    GDK_y,
    OP_CLEAR,
    M_NONE, 
    do_business
},
{ 
    N_("Term"),
    N_(
      "Term\n"
      "\n"
      "Payment period\n"
      "\n"
      "Keyboard equivalent:   T\n"
      "\n"
      "Returns the number of payment periods in the term\n"
      "of an ordinary annuity necessary to accumulate a\n"
      "future value of fv, earning a periodic interest\n"
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
      "You deposit $1,800 at the end of each year into\n"
      "a bank account. Your account earns 11% a year,\n"
      "compounded annually. You want to determine how\n"
      "long it will take to accumulate $120,000.\n"
      "\n"
      "Memory register usage:\n"
      "\n"
      "   Register 0 - 1800    (periodic payment)\n"
      "   Register 1 - 120000  (future value)\n"
      "   Register 2 - 0.11    (periodic interest rate\n"
      "             is 11%)\n"
      "\n"
      "Clicking SELECT on Term returns 20.32, the number\n"
      "of years it will take to accumulate $120,000 in\n"
      "your account."
    ),
    0,
    GDK_T,
    OP_CLEAR,
    M_NONE,
    do_business
},
{ 
    "    ",
    "    ", 
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ", 
    0,
    0,
    OP_NOP,
    M_NONE, 
    do_none
},
{ 
    "    ",
    "    ", 
    0,
    0,
    OP_NOP,
    M_NONE, 
    do_none
},
{ 
    "    ",
    "    ", 
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ", 
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},

/* Logical. */
{ 
    N_("<"),
    N_(
      "<\n"
      "\n"
      "Left shift n\n"
      "\n"
      "Keyboard equivalent:   <\n"
      "\n"
      "Use this key to shift the displayed binary value\n"
      "a designated number of places to the left. Click\n"
      "SELECT on the key, then on a digit in the range\n"
      "0 to f to indicate how many places to shift."
    ),
    GDK_SHIFT_MASK, 
    GDK_less,
    OP_SET,
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
      "Use this key to shift the displayed binary value\n"
      "a designated number of places to the right.\n"
      "Click SELECT on the key, then on a digit in the\n"
      "range 0 to f to indicate how many places to shift."
    ),
    GDK_SHIFT_MASK, 
    GDK_greater,
    OP_SET,
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
      "This is a logical function that truncates the\n"
      "given number and returns a 16-bit unsigned\n"
      "integer."
    ),
    0,
    GDK_bracketleft,
    OP_CLEAR, 
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
      "This is a logical function that truncates the\n"
      "given number and returns a 32-bit unsigned\n"
      "integer."
    ),
    0,
    GDK_bracketright, 
    OP_CLEAR,
    M_NONE,
    do_immed
},
{ 
    "    ",
    "    ", 
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ", 
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ", 
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ", 
    0,
    0,
    OP_NOP,
    M_NONE, 
    do_none
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
      "This key performs a logical OR operation on the\n"
      "last number entered and the next number entered,\n"
      "treating both numbers as unsigned long integers."
    ),
    GDK_SHIFT_MASK, 
    GDK_bar,
    OP_SET,
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
      "This key performs a logical AND operation on the\n"
      "last number entered and the next number entered,\n"
      "treating both numbers as unsigned long integers."
    ),
    GDK_SHIFT_MASK, 
    GDK_ampersand,
    OP_SET,
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
      "This key performs a logical NOT operation on the\n"
      "current displayed value."
    ),
    GDK_SHIFT_MASK, 
    GDK_asciitilde,
    OP_CLEAR, 
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
      "This key takes the last number entered and the\n"
      "next number entered, and performs a logical XOR\n"
      "operation on them, treating both numbers as\n"
      "unsigned long integers."
    ),
    GDK_SHIFT_MASK, 
    GDK_caret,
    OP_SET,
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
      "This key takes the last number entered and the\n"
      "next number entered, and performs a logical XNOR\n"
      "operation on them, treating both numbers as\n"
      "unsigned long integers."
    ),
    0,
    GDK_n,
    OP_SET,
    M_NONE,
    do_calc
},
{ 
    "    ",
    "    ",
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    0,
    0,
    OP_NOP,
    M_NONE, 
    do_none
},
{ 
    "    ",
    "    ", 
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},

/* Scientific. */
{ 
    N_("Trig"),
    N_(
      "Trig\n"
      "\n"
      "Sets trigonometric mode.\n"
      "\n"
      "Keyboard equivalent:   T\n"
      "\n"
      "Initially the trigonometric display type is\n"
      "degrees. You can change this to gradients or\n"
      "radians.\n"
      "\n"
      "Click on the Trig button to show the available settings.\n"
      "Choose a value from the menu to set the new\n"
      "trigonometric display type.\n"
      "\n"
      "Alternatively, you can use the keyboard to set the\n"
      "trigonometric display type.\n"
      "\n"
      "Type T, followed by d, g, or r to indicate the\n"
      "new trigonometric display setting:\n"
      "\n"
      "d   degrees\n"
      "g   gradients\n"
      "r   radians"
    ),
    GDK_SHIFT_MASK,
    GDK_T,
    OP_SET,
    M_TRIG,
    do_pending
},
{ 
    N_("Hyp"),
    N_(
      "Hyp\n"
      "\n"
      "Hyperbolic flag\n"
      "\n"
      "Keyboard equivalent:   h\n"
      "\n"
      "This key is a toggle that sets and unsets the\n"
      "hyperbolic function flag. That flag affects\n"
      "Sin, Cos, and Tan trigonometric functions."
    ),
    0,
    GDK_h,
    OP_CLEAR,
    M_NONE,
    do_immed
},
{ 
    N_("Inv"),
    N_(
      "Inv\n"
      "\n"
      "Inverse function flag\n"
      "\n"
      "Keyboard equivalent:   i\n"
      "\n"
      "This key is a toggle for setting or unsetting\n"
      "the inverse function flag. This flag affects Sin,\n"
      "Cos, and Tan trigonometric functions."
    ),
    0,
    GDK_i,
    OP_CLEAR,
    M_NONE,
    do_immed
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
      "Returns e raised to the power of the current\n"
      "displayed value."
    ),
    GDK_SHIFT_MASK,
    GDK_braceleft,
    OP_CLEAR,
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
      "Returns 10 raised to the power of the current\n"
      "displayed value."
    ),
    GDK_SHIFT_MASK,
    GDK_braceright,
    OP_CLEAR,
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
      "Takes the last number entered and raises it to\n"
      "the power of the next number entered.\n"
      "\n"
      "Example:\n"
      "\n"
      "        2 Y 3 = (returns 8)"
    ),
    0,
    GDK_y,
    OP_SET,
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
      "Returns the factorial of the current displayed\n"
      "value. This will only work for positive integer\n"
      "values."
    ),
    GDK_SHIFT_MASK,
    GDK_exclam,
    OP_CLEAR,
    M_NONE,
    do_immed
},
{ 
    "    ",
    "    ",
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},
{ 
    N_("Cos"),
    N_(
      "Cos\n"
      "\n"
      "Cosine function\n"
      "\n"
      "Keyboard equivalent:   Control-c\n"
      "\n"
      "Returns the trigonometric cosine, arc cosine,\n"
      "hyperbolic cosine, or inverse hyperbolic cosine\n"
      "of the current displayed value, depending on the\n"
      "settings of the Hyp and Inv flags.\n"
      "\n"
      "The result appears in the current trigonometric\n"
      "unit (degrees, radians, or gradients)."
    ),
    GDK_CONTROL_MASK,
    GDK_c,
    OP_CLEAR,
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
      "Keyboard equivalent:   Control-s\n"
      "\n"
      "Returns the trigonometric sine, arc sine,\n"
      "hyperbolic sine, or inverse hyperbolic sine of\n"
      "the current displayed value, depending on the\n"
      "settings of the Hyp and Inv flags.\n"
      "\n"
      "The result appears in the current trigonometric\n"
      "unit (degrees, radians, or gradients)."
    ),
    GDK_CONTROL_MASK,
    GDK_s,
    OP_CLEAR,
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
      "Keyboard equivalent:   Control-t\n"
      "\n"
      "Returns the trigonometric tangent, arc tangent,\n"
      "hyperbolic tangent, or inverse hyperbolic tangent\n"
      "of the current displayed value, depending on the\n"
      "settings of the Hyp and Inv flags.\n"
      "\n"
      "The result appears in the current trigonometric\n"
      "unit (degrees, radians, or gradients)."
    ),
    GDK_CONTROL_MASK,
    GDK_t,
    OP_CLEAR,
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
      "Returns the natural logarithm of the current\n"
      "displayed value."
    ),
    GDK_SHIFT_MASK,
    GDK_N,
    OP_CLEAR,
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
      "Returns the base 10 logarithm of the current\n"
      "displayed value."
    ),
    GDK_SHIFT_MASK,
    GDK_G,
    OP_CLEAR,
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
      "Generates a random number in the range 0.0 to 1.0\n"
      "and enters it into the calculator display."
    ),
    GDK_SHIFT_MASK,
    GDK_question,
    OP_CLEAR,
    M_NONE,
    do_immed
},
{ 
    "    ",
    "    ",
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
},
{ 
    "    ",
    "    ",
    0,
    0,
    OP_NOP,
    M_NONE,
    do_none
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

    init_text();               /* Setup text strings depending upon language. */
    init_vars();               /* Setup default values for variables. */
    load_resources();          /* Get resources from various places. */
    read_resources();          /* Read resources from merged database. */
    get_options(argc, argv);   /* Get command line arguments. */
    read_rcfiles();            /* Read .calctoolrc's files. */
    switch_hands(v->righthand);
    make_frames();             /* Create gcalctool window frames. */

    v->shelf      = NULL;      /* No selection for shelf initially. */
    v->noparens   = 0;         /* No unmatched brackets initially. */
    v->opsptr     = 0;         /* Nothing on the parentheses op stack. */
    v->numsptr    = 0;         /* Nothing on the parenthese numeric stack. */
    v->pending    = 0;         /* No initial pending command. */
    v->tstate     = 0;         /* Button values displayed first. */
    v->hyperbolic = 0;         /* Normal trig functions initially. */
    v->inverse    = 0;         /* No inverse functions initially. */
    v->down       = 0;         /* No mouse presses initially. */

    srand48((long) time((time_t *) 0));   /* Seed random number generator. */

    do_clear();                /* Initialise and clear display. */

    if (v->rstate == TRUE) {   /* Show the memory register window? */
        make_registers();
        if (!v->iconic) win_display(FCP_REG, TRUE);
    }
    if (v->modetype != BASIC) {     /* Show the mode window? */
        set_title(FCP_MODE, mstrs[(int) v->modetype]);
        set_item(MODEITEM, mode_str[(int) v->modetype]);
        if (!v->iconic) {
            win_display(FCP_MODE, TRUE);
        }
    }

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
    set_item(DISPLAYITEM, v->display);
    v->error = 1;
    if (v->beep == TRUE) {
        beep();
    }
    set_item(OPITEM, _("CLR"));
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


void
switch_hands(int righthand)
{
    int i;

    for (i = 0; i < BCOLS; i++) {
        cur_pos[i] = (righthand) ? right_pos[i] : left_pos[i];
    }
}
