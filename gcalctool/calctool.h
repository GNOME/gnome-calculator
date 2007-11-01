
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

#ifndef CALCTOOL_H
#define CALCTOOL_H

#include "config.h"

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <locale.h>
#include <math.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#define MP_SIZE      1000     /* Size of the multiple precision values. */

#define FCLOSE       (void) fclose     /* To make lint happy. */
#define FPRINTF      (void) fprintf
#define FPUTS        (void) fputs
#define GETHOSTNAME  (void) gethostname
#define MEMCPY       (void) memcpy
#define MEMSET       (void) memset
#define MKSTEMP      (void) mkstemp
#define REWIND       (void) rewind
#define SNPRINTF     (void) snprintf
#define SPRINTF      (void) sprintf
#define SSCANF       (void) sscanf
#define STRCAT       (void) strcat
#define STRCPY       (void) strcpy
#define STRNCPY      (void) strncpy
#define STRNCAT      (void) strncat
#define UNLINK       (void) unlink

/* Menu bar menu types. */

enum mb_type { M_ABOUT, M_ASCII, M_BASIC, M_ADV, M_CONTENTS, M_COPY,  M_FIN,
	       M_PASTE, M_QUIT,  M_REGS,  M_SCI, M_EXP, M_TSEP, M_ZEROES,
               M_LR_ARITH, M_OP_ARITH };

enum base_type { BIN, OCT, DEC, HEX };      /* Base definitions. */

/* Main calctool window types. */
enum fcp_type  { FCP_KEY, FCP_REG, FCP_MODE };

enum item_type { BASEITEM, TTYPEITEM, NUMITEM,
                 HYPITEM,  INVITEM,   OPITEM,  MODEITEM };

/* Calculator modes. */
enum mode_type { BASIC, ADVANCED, FINANCIAL, SCIENTIFIC };

enum num_type { ENG, FIX, SCI };            /* Number display mode. */

/* Resources. */
enum res_type { R_ACCURACY, R_BASE, R_DISPLAY, R_MODE, R_REGS, R_TRIG,
                R_ZEROES,   R_TSEP, R_SYNTAX,  R_XPOS, R_YPOS,
                R_REG0,     R_REG1, R_REG2,    R_REG3, R_REG4,
                R_REG5,     R_REG6, R_REG7,    R_REG8, R_REG9,
				R_BITCALC
};

enum trig_type { DEG, GRAD, RAD };          /* Trigonometric types. */

enum trig_func {SIN=0, COS=1, TAN=2};       

/* Abbreviations for the gcalctool keyboard */

enum
{
    KEY_0,
    KEY_1,
    KEY_2,
    KEY_3,
    KEY_4,
    KEY_5,
    KEY_6,
    KEY_7,
    KEY_8,
    KEY_9,
    KEY_A,
    KEY_B,
    KEY_C,
    KEY_D,
    KEY_E,
    KEY_F,
    KEY_NUMERIC_POINT,
    KEY_CALCULATE,
    KEY_CLEAR,
    KEY_CLEAR_ENTRY,
    KEY_START_BLOCK,
    KEY_END_BLOCK,
    KEY_ADD,
    KEY_SUBTRACT,
    KEY_MULTIPLY,
    KEY_DIVIDE,
    KEY_BACKSPACE,
    KEY_CHANGE_SIGN,
    KEY_INTEGER,
    KEY_FRACTION,
    KEY_PERCENTAGE,
    KEY_SQUARE,
    KEY_SQUARE_ROOT,
    KEY_RECIPROCAL,
    KEY_E_POW_X,
    KEY_10_POW_X,
    KEY_X_POW_Y,
    KEY_FACTORIAL,
    KEY_RANDOM,
    KEY_SINE,
    KEY_COSINE,
    KEY_TANGENT,
    KEY_NATURAL_LOGARITHM,
    KEY_LOGARITHM,
    KEY_ABSOLUTE_VALUE,
    KEY_MASK_16,
    KEY_MASK_32,
    KEY_MODULUS_DIVIDE,
    KEY_EXPONENTIAL,
    KEY_NOT,
    KEY_OR,
    KEY_AND,
    KEY_XOR,
    KEY_XNOR,
    KEY_FINC_CTRM,
    KEY_FINC_DDB,
    KEY_FINC_FV,
    KEY_FINC_PMT,
    KEY_FINC_PV,
    KEY_FINC_RATE,
    KEY_FINC_SLN,
    KEY_FINC_SYD,
    KEY_FINC_TERM,
    KEY_LEFT_SHIFT,
    KEY_RIGHT_SHIFT,
    KEY_STORE,
    KEY_RECALL,
    KEY_EXCHANGE,
    KEY_SET_ACCURACY,
    KEY_CONSTANT,
    KEY_FUNCTION,
    NKEYS
};

#define EQUAL(a, b)    (strlen(a)==strlen(b)) & !strcmp(a, b) 

#define INC            { argc--; argv++; }

#ifndef LINT_CAST
#ifdef  lint
#define LINT_CAST(arg)  (arg ? 0 : 0)
#else
#define LINT_CAST(arg)  (arg)
#endif /*lint*/
#endif /*LINT_CAST*/

#define MAX_DIGITS     200         /* Maximum displayable number of digits. */
#define MAX_LOCALIZED  (MAX_DIGITS * (1 + MB_LEN_MAX) + MB_LEN_MAX)

#ifndef MAXLINE
#define MAXLINE        512        /* Length of character strings. */
#endif /*MAXLINE*/

#define MAXACC         99         /* Max. number of digits after numeric point. */
#define MAXBASES       4          /* Maximum number of numeric bases. */
#define MAXCONFUN      10         /* Maximum number of constants/functions. */
#define MAXDISPMODES   3          /* Maximum number of display modes. */
#define MAXMODES       4          /* Maximum number of calculator modes. */
#define MAXREGS        10         /* Maximum number of memory registers. */
#define MAXSTACK       256        /* Parenthese stack size. */
#define MAXTRIGMODES   3          /* Maximum number of trig. modes. */
#define MAXSYNTAX      2          /* Number of syntaxes in calculator */
#define MAXBITCALC     2          /* Choices for bitcalculating */

#ifndef MIN
#define MIN(x,y)       ((x) < (y) ? (x) : (y))
#endif /*MIN*/

#ifndef CFNAME
#define CFNAME         ".gcalctoolcf"
#endif /*CFNAME*/

#ifndef RCNAME
#define RCNAME         ".gcalctoolrc"
#endif /*RCNAME*/

#undef TRUE                    /* Boolean definitions. */
#define TRUE           1

#undef FALSE
#define FALSE          0

#define UNDO_HISTORY_LENGTH 16  /* Arithmetic mode undo history length */

#define MPMATH_ERR		    	20001

typedef unsigned long  BOOLEAN;

enum button_flags {
    none         = 0,          /* No flags */
    enter        = (1 << 2),   /* Expression is entered */
    number       = (1 << 3),   /* Number button */
    func         = (1 << 6),   /* Function */
    bsp          = (1 << 7),   /* Backspace */
    clear        = (1 << 8),   /* Clear display */
    neg          = (1 << 9),   /* Negate display */
    inv          = (1 << 10),  /* Reciprocial */
    con          = (1 << 11),  /* Constant */
    regrcl       = (1 << 12),  /* Recall register */
    expnum       = (1 << 13),  /* Exponential number */
    postfixop    = (1 << 14),  /* Unary postfix operation */
    prefixop     = (1 << 15),  /* Unary prefix operation */
    dpoint       = (1 << 16)   /* Decimal point */
};

enum shiftd {
    left = 0,
    right 
};

enum syntax {
    npa = 0,                 /* Non-precedence arithmetic */
    exprs,                   /* Expression with arithmetic precedence */
};

struct button {
    int id;
    char *symname;           /* Expression function name */
    void (*func)();          /* Function to obey on button press. */
    enum button_flags flags; /* Misc flags */
};

struct exprm_state {       /* Expression mode state */
    struct button button;  /* Current button/character pressed. */
    int value;
    int ans[MP_SIZE];      /* Previously calculated answer */
    char *expression;      /* Expression entered by user */
};

/* Circular list of Arithmetic Precedence Mode states*/ 
struct exprm_state_history {
  unsigned int begin;
  unsigned int end;
  unsigned int current;
  struct exprm_state e[UNDO_HISTORY_LENGTH];  /* Expression mode state */
};

struct menu {
    char *title;             /* Menu title. */
    int  total;              /* Number of menu entries. */
    int  index;              /* Index into menu string array. */
    int  defval;             /* Default menu item position (from 1). */
};

struct calcVars {                      /* Calctool variables and options. */
    struct exprm_state_history h;      /* History of expression mode states */

    int current;                       /* Current button/character pressed. */
  
    char *appname;                     /* Application name for resources. */
    char con_names[MAXREGS][MAXLINE];  /* Selectable constant names. */
    char display[MAXLINE];             /* Current calculator display. */
    char *exp_posn;                    /* Position of the exponent sign. */
    char fnum[MAX_DIGITS];             /* Scratchpad for fixed numbers. */
    char fun_names[MAXREGS][MAXLINE];  /* Function names from .gcalctoolcf. */
    char fun_vals[MAXREGS][MAXLINE];   /* Function defs from .gcalctoolcf. */
    char *home;                        /* Pathname for users home directory. */
    char *iconlabel;                   /* The calctool icon label. */
    char op_item_text[5];              /* Operand item panel string. */
    char opstr[5];                     /* Operand string during pending op. */
    char *progname;                    /* Name of this program. */
    char pstr[5];                      /* Current button text string. */
    const char *radix;                 /* Locale specific radix string. */
    char *shelf;                       /* PUT selection shelf contents. */
    char snum[MAX_DIGITS];             /* Scratchpad for scientific numbers. */
    const char *tsep;                  /* Locale specific thousands seperator. */
    char *titleline;                   /* Value of titleline (if present). */
    char *tool_label;                  /* Title line for calculator window. */

    int MPcon_vals[MAXREGS][MP_SIZE];  /* Selectable constants. */
    int MPdebug;                       /* If set, debug info. to stderr. */
    int MPerrors;                      /* If set, output errors to stderr. */
    int MPdisp_val[MP_SIZE];           /* Value of the current display. */
    int MPexpr_val[MP_SIZE];           /* Value of the current expression. */
    int MPlast_input[MP_SIZE];         /* Previous number input by user. */
    int MPmvals[MAXREGS][MP_SIZE];     /* Memory register values. */
    int *MPnumstack[MAXSTACK];         /* Numeric stack for parens. */
    int MPresult[MP_SIZE];             /* Current calculator total value. */
    int MPimresult[MP_SIZE];           /* Current intermediate result. */
    int MPtresults[3][MP_SIZE];        /* Current trigonometric results. */

    enum base_type base;            /* Current base: BIN, OCT, DEC or HEX. */
    enum fcp_type curwin;           /* Window current event occured in. */
    enum mode_type modetype;        /* Current calculator mode. */
    enum num_type dtype;            /* Number display mode. */
    enum trig_type ttype;           /* Trig. type (deg, grad or rad). */

    enum syntax syntax;             /* Calculation syntax mode */

    int accuracy;      /* Number of digits precision (Max 9). */
    int beep;          /* Indicates whether there is a beep sound on error. */
    int cur_op;        /* Current arithmetic operation. */
    int doing_mi;      /* Set if adjusting the "show zeroes" menu item. */
    int error;         /* Indicates some kind of display error. */
    int ghost_zero;    /* Flag to indicate display with "0", actually 
                           having empty content. */
    int math_error;    /* Math error (used in expression mode) */
    int hyperbolic;    /* If set, trig functions will be hyperbolic. */
    int iconic;        /* Set if window is currently iconic. */
    int inverse;       /* If set, trig and log functions will be inversed. */
    int key_exp;       /* Set if entering exponent number. */
    int ndisplay;      /* Height of the numerical display. */
    int new_input;     /* New number input since last op. */
    int noparens;      /* Count of left brackets still to be matched. */
    int numsptr;       /* Pointer into the parenthese numeric stack. */
    int old_cal_value;      /* Previous calculation operator. */
    int opsptr;        /* Pointer into the parentheses operand stack. */
    int opstack[MAXSTACK];  /* Stack containing parentheses input. */
    int pointed;       /* Whether a decimal point has been given. */
    int rstate;        /* Indicates if memory register frame is displayed. */
    int show_paren;    /* Set if we wish to show DISPLAYITEM during parens. */
    int show_tsep;     /* Set if the thousands seperator should be shown. */
    int show_zeroes;   /* Set if trailing zeroes should be shown. */
    int started;       /* Set just before window is displayed. */
    int toclear;       /* Indicates if display should be cleared. */
    int warn_change_mode;    /* Should we warn user when changing modes? */
    int bitcalculating_mode;  /* 0 = no, else yes */
};

typedef struct calcVars *Vars;


/* MP definitions. */

#define C_abs(x)    ((x) >= 0 ? (x) : -(x))
#define dabs(x)     (double) C_abs(x)
#define min(a, b)   ((a) <= (b) ? (a) : (b))
#define max(a, b)   ((a) >= (b) ? (a) : (b))
#define dmax(a, b)  (double) max(a, b)
#define dmin(a, b)  (double) min(a, b)

struct button *button_for_value(int);

void syntaxdep_show_display();
char *button_str(int);
char *convert(char *);
char *gc_strdup(char *str);
const char *get_radix();
char *get_resource(enum res_type);
const char *get_tsep();
char *make_fixed(int *, char *, int, int, int);
char *make_number(int *, int, BOOLEAN);
char *set_bool(int);

char *get_localized_numeric_point(void);

unsigned short *get_but_data();

int button_mods(int);
int button_value(int);
int do_rcl_reg(int reg, int value[MP_SIZE]);
int do_sto_reg(int reg, int value[MP_SIZE]);
int do_tfunc(int s[MP_SIZE], int t[MP_SIZE], enum trig_func tfunc);
int get_int_resource(enum res_type, int *);
int main(int, char **);

void beep();
void build_word_map();
void clear_display(int);
void display_prop_sheet();
void do_base(enum base_type);
void do_business();
void do_calc();
void do_lr_calc();
void do_expression();
void do_calctool(int, char **);
void do_clear();
void do_clear_entry();
void do_delete();
void do_numtype(enum num_type);
void do_expno();
void do_immed();
void do_memory();
void do_mode(int);
void do_number();
void do_paren();
void do_lshift();
void do_rshift();
void do_sto();
void do_rcl();
void do_exchange();
void do_accuracy();
void do_constant();
void do_function();
void do_point();
void do_portion();
void do_trig();
void do_trigtype(enum trig_type);
void do_percent();
void doerr(char *);
void exp_del();
void get_constant(int);
void get_function(int);
void get_options(int, char **);
void grey_buttons(enum base_type);
void initialise();
void init_args();
void init_frame_sizes();
void init_vars();
void init_Xvars(int *, char **);
void key_init();
void localize_number(char *, const char *);
void load_resources();
void make_frames();
void make_menus();
void make_reg(int, char *);
void make_registers();
void mperr();
void MPstr_to_num(char *, enum base_type, int *);
void paren_disp(int);
void process_item(struct button *, int);
void process_str(char *);
void put_resource(enum res_type, char *);
void read_cfdefs();
void read_resources();
void read_str(char **, char *);
void refresh_display();
void save_resources();
void set_accuracy_menu_item(int);
void set_accuracy_tooltip(int);
void set_display(char *, int);
void write_display(char *);
void set_error_state(int);
void set_hyp_item(int);
void set_ins_key();
void set_inv_item(int);
void set_main_title(enum mode_type);
void set_mode(enum mode_type);
void set_title(enum fcp_type, char *);
void show_change_mode_dialog();
void show_display(int *);
void show_error(char *);
void srand48();
void start_tool();
void str_replace(char **, char *, char *);
void switch_hands(int);
void update_statusbar(gchar *, const gchar *);
void usage(char *);
void win_display(enum fcp_type, int);
void set_redo_and_undo_button_sensitivity(int undo, int redo);

/* Global Brent MP routines in mp.c. */
int mpeq(int *, int *);
int mpge(int *, int *);
int mpgt(int *, int *);
int mple(int *, int *);
int mplt(int *, int *);

void mpabs(int *, int *);
void mpadd(int *, int *, int *);
void mpaddi(int *, int *, int *);
void mpasin(int *, int *);
void mpatan(int *, int *);
void mpcdm(double *, int *);
void mpcim(int *, int *);
void mpcmd(int *, double *);
void mpcmf(int *, int *);
void mpcmi(int *, int *);
void mpcmim(int *, int *);
void mpcos(int *, int *);
void mpcosh(int *, int *);
void mpdiv(int *, int *, int *);
void mpdivi(int *, int *, int *);
void mpexp(int *, int *);
void mpln(int *, int *);
void mpmul(int *, int *, int *);
void mpmuli(int *, int *, int *);
void mpneg(int *, int *);
void mppi(int *);
void mppwr(int *, int *, int *);
void mppwr2(int *, int *, int *);
void mpset(int *, int *, int *);
void mpsin(int *, int *);
void mpsinh(int *, int *);
void mpsqrt(int *, int *);
void mpstr(int *, int *);
void mpsub(int *, int *, int *);
void mptanh(int *, int *);

void make_exp(char *number, int t[MP_SIZE]);
void exp_replace(char *text);
void insert_to_cursor(char *text);
void get_expr_from_display();
void delete_from_cursor();

#endif /*CALCTOOL_H*/
