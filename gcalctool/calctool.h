
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

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <locale.h>
#include <math.h>
#include <gdk/gdktypes.h>
#include <gdk/gdkkeysyms.h>

#include "config.h"

/* Standard gettext macros. */

#ifdef ENABLE_NLS
#include <libintl.h>
#undef _
#define _(String) dgettext (GETTEXT_PACKAGE, String)
#undef N_
#define N_(String) (String)
#else
#define textdomain(String) (String)
#define gettext(String) (String)
#define dgettext(Domain,Message) (Message)
#define dcgettext(Domain,Message,Type) (Message)
#define bindtextdomain(Domain,Directory) (Domain)
#define _(String) (String)
#define N_(String) (String)
#endif /*ENABLE_NLS*/

#define MP_SIZE      150     /* Size of the multiple precision values. */

#define FCLOSE       (void) fclose     /* To make lint happy. */
#define FPRINTF      (void) fprintf
#define FPUTS        (void) fputs
#define GETHOSTNAME  (void) gethostname
#define MEMCPY       (void) memcpy
#define MEMSET       (void) memset
#define MKTEMP       (void) mktemp
#define REWIND       (void) rewind
#define SPRINTF      (void) sprintf
#define SSCANF       (void) sscanf
#define STRCAT       (void) strcat
#define STRCPY       (void) strcpy
#define STRNCAT      (void) strncat
#define UNLINK       (void) unlink

/* Various pseudo events used by the calctool program. */
#define KEYBOARD_DOWN    100    /* Keyboard character was pressed. */
#define KEYBOARD_UP      101    /* Keyboard character was released. */
#define LEFT_DOWN        102    /* Left mouse button was depressed. */
#define LEFT_UP          103    /* Left mouse button was debounced. */
#define MIDDLE_DOWN      104    /* Middle mouse button was depressed. */
#define MIDDLE_UP        105    /* Middle mouse button was debounced. */
#define RIGHT_DOWN       106    /* Right mouse button was depressed. */
#define RIGHT_UP         107    /* Right mouse button was debounced. */
#define TAKE_FROM_SHELF  108    /* PUT function key was pressed. */
#define PUT_ON_SHELF     109    /* GET function key was pressed. */
#define SHOWHELP         110    /* F1 key was pressed; show help message. */
#define LASTEVENTPLUSONE 111    /* Not one of the above. */

/* Menu bar menu types. */
enum mb_type { M_ABOUT, M_ASCII, M_BASIC, M_CONTENTS, 
               M_COPY,  M_FIN,   M_QUIT,  M_REGS,     M_SCI };

enum base_type { BIN, OCT, DEC, HEX };      /* Base definitions. */

/* Main calctool window types. */
enum fcp_type  { FCP_KEY, FCP_REG, FCP_MODE };

enum item_type { BASEITEM, TTYPEITEM, NUMITEM,
                 HYPITEM,  INVITEM,   OPITEM,  MODEITEM };

/* Popup menu types. */
enum menu_type { M_ACC,  M_CON,  M_EXCH, M_FUN,  M_LSHF,
                 M_RCL,  M_RSHF, M_STO,  M_NONE
};

/* Calculator modes. */
enum mode_type { BASIC, FINANCIAL, SCIENTIFIC };

enum num_type { ENG, FIX, SCI };            /* Number display mode. */

enum op_type { OP_SET, OP_CLEAR, OP_NOP };  /* Operation item settings. */

/* Resources. */
enum res_type { R_ACCURACY, R_BASE, R_DISPLAY, R_MODE, R_HELP,
                R_REGS,     R_TRIG, R_BEEP
};

enum trig_type { DEG, GRAD, RAD };          /* Trigonometric types. */

/* Abbreviations for the gcalctool keyboard and menu equivalents. */

#define KEY_7     b_buttons[0].value              /* 7 */
#define KEY_8     b_buttons[1].value              /* 8 */
#define KEY_9     b_buttons[2].value              /* 9 */
#define KEY_DIV   b_buttons[3].value              /* / */
                                                  /* Empty/hidden button. */
#define KEY_BSP   b_buttons[5].value              /* CTL('h') */
#define KEY_CE    b_buttons[6].value              /* CTL(backspace) */
#define KEY_CLR   b_buttons[7].value              /* del */

#define KEY_4     b_buttons[8].value              /* 4 */
#define KEY_5     b_buttons[9].value              /* 5 */
#define KEY_6     b_buttons[10].value             /* 6 */
#define KEY_MUL   b_buttons[11].value             /* * */
#define KEY_ACC   b_buttons[12].value             /* A */
#define KEY_CHS   b_buttons[13].value             /* C */
#define KEY_INT   b_buttons[14].value             /* CTL('i') */
#define KEY_RCL   b_buttons[15].value             /* R */

#define KEY_1     b_buttons[16].value             /* 1 */
#define KEY_2     b_buttons[17].value             /* 2 */
#define KEY_3     b_buttons[18].value             /* 3 */
#define KEY_SUB   b_buttons[19].value             /* - */
#define KEY_PER   b_buttons[20].value             /* % */
#define KEY_SQRT  b_buttons[21].value             /* s */
#define KEY_FRAC  b_buttons[22].value             /* CTL('f') */
#define KEY_STO   b_buttons[23].value             /* S */

#define KEY_0     b_buttons[24].value             /* 0 */
#define KEY_PNT   b_buttons[25].value             /* . */
#define KEY_EQ    b_buttons[26].value             /* = */
#define KEY_ADD   b_buttons[27].value             /* + */
#define KEY_REC   b_buttons[28].value             /* r */
#define KEY_SQR   b_buttons[29].value             /* @ */
#define KEY_ABS   b_buttons[30].value             /* CTL('u') */
#define KEY_EXCH  b_buttons[31].value             /* X */

#define KEY_CTRM  f_buttons[0].value              /* CTL('t') */
#define KEY_DDB   f_buttons[1].value              /* CTL('d') */
#define KEY_FV    f_buttons[2].value              /* v */
#define KEY_PMT   f_buttons[3].value              /* P */
#define KEY_PV    f_buttons[4].value              /* p */
#define KEY_RATE  f_buttons[5].value              /* CTL('r') */
#define KEY_SLN   f_buttons[6].value              /* CTL('s') */
#define KEY_SYD   f_buttons[7].value              /* CTL('y') */
#define KEY_TERM  f_buttons[8].value              /* T */

#define KEY_LSFT  s_buttons[0].value              /* < */
#define KEY_RSFT  s_buttons[1].value              /* > */
#define KEY_16    s_buttons[2].value              /* [ */
#define KEY_32    s_buttons[3].value              /* ] */
                                                  /* Empty/hidden button. */
#define KEY_LPAR  s_buttons[5].value              /* ( */
#define KEY_RPAR  s_buttons[6].value              /* ) */
#define KEY_KEYS  s_buttons[7].value              /* k */

#define KEY_EXP   s_buttons[8].value              /* E */
#define KEY_CON   s_buttons[9].value              /* # */
#define KEY_FUN   s_buttons[10].value             /* F */
#define KEY_ETOX  s_buttons[11].value             /* { */
#define KEY_TTOX  s_buttons[12].value             /* } */
#define KEY_YTOX  s_buttons[13].value             /* y */
#define KEY_FACT  s_buttons[14].value             /* ! */
#define KEY_RAND  s_buttons[15].value             /* ? */

#define KEY_D     s_buttons[16].value             /* d */
#define KEY_E     s_buttons[17].value             /* e */
#define KEY_F     s_buttons[18].value             /* f */
#define KEY_COS   s_buttons[19].value             /* CTL('c') */
#define KEY_SIN   s_buttons[20].value             /* CTL('s') */
#define KEY_TAN   s_buttons[21].value             /* CTL('t') */
#define KEY_LN    s_buttons[22].value             /* N */
#define KEY_LOG   s_buttons[23].value             /* G */

#define KEY_A     s_buttons[24].value             /* a */
#define KEY_B     s_buttons[25].value             /* b */
#define KEY_C     s_buttons[26].value             /* c */
#define KEY_OR    s_buttons[27].value             /* | */
#define KEY_AND   s_buttons[28].value             /* & */
#define KEY_NOT   s_buttons[29].value             /* ~ */
#define KEY_XOR   s_buttons[30].value             /* ^ */
#define KEY_XNOR  s_buttons[31].value             /* n */

#define BCOLS          8      /* No of columns of Basic Mode buttons. */
#define BROWS          4      /* No of rows of Basic Mode buttons. */

#define FCOLS          8      /* No of columns of Financial Mode buttons. */
#define FROWS          2      /* No of rows of Financial Mode buttons. */

#define SCOLS          8      /* No of columns of Scientific Mode buttons. */
#define SROWS          4      /* No of rows of Scientific Mode buttons. */

#define CTL(n)         n - 96     /* Generate control character value. */
#define EQUAL(a, b)    !strncmp(a, b, strlen(b))

#define INC            { argc--; argv++; }
#define IS_KEY(v, n)   (v == n)

#ifndef LINT_CAST
#ifdef  lint
#define LINT_CAST(arg)  (arg ? 0 : 0)
#else
#define LINT_CAST(arg)  (arg)
#endif /*lint*/
#endif /*LINT_CAST*/

#define MAX_DIGITS     40         /* Maximum displayable number of digits. */

/* Maximum number of various graphics pieces. */
#define MAXITEMS       8          /* Maximum number of panel items. */
#define MAXMENUS       9          /* Maximum number of popup menus. */

#ifndef MAXLINE
#define MAXLINE        256        /* Length of character strings. */
#endif /*MAXLINE*/

#define MAXACC         10    /* Max. number of digits+1 after numeric point. */
#define MAXBASES       4          /* Maximum number of numeric bases. */
#define MAXCONFUN      10         /* Maximum number of constants/functions. */
#define MAXDISPMODES   3          /* Maximum number of display modes. */
#define MAXENTRIES     50         /* Maximum number of menu entries. */
#define MAXMODES       3          /* Maximum number of calculator modes. */
#define MAXREGS        10         /* Maximum number of memory registers. */
#define MAXSTACK       256        /* Parenthese stack size. */
#define MAXTRIGMODES   3          /* Maximum number of trig. modes. */
#define MAXVKEYS       5          /* Number of valid keys after an error. */

#define MCOLS          8          /* Number of columns of "special" keys. */
#define MROWS          2          /* Number of rows of "special" keys. */
#define MODEKEYS       MCOLS * MROWS

#ifndef MIN
#define MIN(x,y)       ((x) < (y) ? (x) : (y))
#endif /*MIN*/

#define B_NOBUTTONS      BROWS * BCOLS  /* Number of Basic Mode buttons. */
#define F_NOBUTTONS      FROWS * FCOLS  /* Number of Financial Mode buttons. */
#define S_NOBUTTONS      SROWS * SCOLS  /* Number of Scientific Mode buttons. */

#ifndef CFNAME
#define CFNAME         ".gcalctoolcf"
#endif /*CFNAME*/

#ifndef RCNAME
#define RCNAME         ".gcalctoolrc"
#endif /*RCNAME*/

#ifndef TRUE                    /* Boolean definitions. */
#define TRUE           1
#endif /*TRUE*/

#ifndef FALSE
#define FALSE          0
#endif /*FALSE*/

typedef unsigned long  BOOLEAN;

struct button {
    char *str;               /* Button display string. */
    char *hstr;              /* Button help string. */
    int mods;                /* Keyboard modifiers (Shift, Ctrl, ...). */
    int value;               /* Unique button keyboard equivalent. */
    enum op_type opdisp;     /* Is button selected during operation? */
    enum menu_type mtype;    /* Type of popup menu (if any). */
    void (*func)();          /* Function to obey on button press. */
};

struct menu {
    char *title;             /* Menu title. */
    int  total;              /* Number of menu entries. */
    int  index;              /* Index into menu string array. */
    int  defval;             /* Default menu item position (from 1). */
};

struct calcVars {                      /* Calctool variables and options. */
    struct button *pending_but;        /* Button info. for pending op. */
    struct button *current;            /* Current button/character pressed. */

    char *appname;                     /* Application name for resources. */
    char con_names[MAXREGS][MAXLINE];  /* Selectable constant names. */
    char cur_op;                       /* Current arithmetic operation. */
    char display[MAXLINE];             /* Current calculator display. */
    char *exp_posn;                    /* Position of the exponent sign. */
    char fnum[MAX_DIGITS+1];           /* Scratchpad for fixed numbers. */
    char fun_names[MAXREGS][MAXLINE];  /* Function names from .gcalctoolcf. */
    char fun_vals[MAXREGS][MAXLINE];   /* Function defs from .gcalctoolcf. */
    char *home;                        /* Pathname for users home directory. */
    char *iconlabel;                   /* The calctool icon label. */
    char old_cal_value;                /* Previous calculation operator. */
    char op_item_text[5];              /* Operand item panel string. */
    char opstr[5];                     /* Operand string during pending op. */
    char *progname;                    /* Name of this program. */
    char pstr[5];                      /* Current button text string. */
    char *shelf;                       /* PUT selection shelf contents. */
    char snum[MAX_DIGITS+1];           /* Scratchpad for scientific numbers. */
    char *titleline;                   /* Value of titleline (if present). */
    char *tool_label;                  /* Title line for calculator window. */

    int MPcon_vals[MAXREGS][MP_SIZE];  /* Selectable constants. */
    int MPdebug;                       /* If set, debug info. to stderr. */
    int MPerrors;                      /* If set, output errors to stderr. */
    int MPdisp_val[MP_SIZE];           /* Value of the current display. */
    int MPlast_input[MP_SIZE];         /* Previous number input by user. */
    int MPmvals[MAXREGS][MP_SIZE];     /* Memory register values. */
    int *MPnumstack[MAXSTACK];         /* Numeric stack for parens. */
    int MPresult[MP_SIZE];             /* Current calculator total value. */
    int MPtresults[3][MP_SIZE];        /* Current trigonometric results. */

    enum base_type base;            /* Current base: BIN, OCT, DEC or HEX. */
    enum fcp_type curwin;           /* Window current event occured in. */
    enum mode_type modetype;        /* Current calculator mode. */
    enum num_type dtype;            /* Number display mode. */
    enum trig_type ttype;           /* Trig. type (deg, grad or rad). */

    int accuracy;      /* Number of digits precision (Max 9). */
    int beep;          /* Indicates whether there is a beep sound on error. */
    int cur_ch;        /* Current character if keyboard event. */
    int curx;          /* Current mouse X position. */
    int cury;          /* Current mouse Y position. */
    int down;          /* Indicates is a mouse button is down. */
    int error;         /* Indicates some kind of display error. */
    int event_type;    /* Type of event being currently processed. */
    int hasicon;       /* Set if user gave icon name on command line. */
    int hyperbolic;    /* If set, trig functions will be hyperbolic. */
    int iconic;        /* Set if window is currently iconic. */
    int iheight;       /* Initial height of the calctool window. */
    int inverse;       /* If set, trig and log functions will be inversed. */
    int ismenu;        /* Set when do_pending called via a popup menu. */
    int iwidth;        /* Initial width of the calctool window. */
    int ix;            /* Initial X position of the icon. */
    int iy;            /* Initial Y position of the icon. */
    int key_exp;       /* Set if entering exponent number. */
    int ndisplay;      /* Height of the numerical display. */
    int new_input;     /* New number input since last op. */
    int noparens;      /* Count of left brackets still to be matched. */
    int numsptr;       /* Pointer into the parenthese numeric stack. */
    int oldx;          /* X position of previous left mouse down. */
    int oldy;          /* Y position of previous left mouse down. */
    int opsptr;        /* Pointer into the parentheses operand stack. */
    int opstack[MAXSTACK];  /* Stack containing parentheses input. */
    int pending;            /* Set for command depending on multiple presses. */
    int pending_op;    /* Arithmetic operation for pending command. */
    int pointed;       /* Whether a decimal point has been given. */
    int rstate;        /* Indicates if memory register frame is displayed. */
    int show_help;     /* Set if we wish to show tooltip help. */
    int show_paren;    /* Set if we wish to show DISPLAYITEM during parens. */
    int started;       /* Set just before window is displayed. */
    int toclear;       /* Indicates if display should be cleared. */
    int wx;            /* Initial X position of the window. */
    int wy;            /* Initial Y position of the window. */
} CalcVars;

typedef struct calcVars *Vars;

/* MP definitions. */

#define C_abs(x)    ((x) >= 0 ? (x) : -(x))
#define dabs(x)     (double) C_abs(x)
#define min(a, b)   ((a) <= (b) ? (a) : (b))
#define max(a, b)   ((a) >= (b) ? (a) : (b))
#define dmax(a, b)  (double) max(a, b)
#define dmin(a, b)  (double) min(a, b)

struct button *button_for_value(int);
struct button *copy_button_info(struct button *);

char *button_str(int);
char *convert(char *);
char *get_resource(enum res_type);
char *make_number(int *, BOOLEAN);

unsigned short *get_but_data();

int button_mods(int);
int button_value(int);
int char_val(char);
int get_menu_entry(enum menu_type, int);
int main(int, char **);

void beep();
void clear_display(int);
void display_prop_sheet();
void do_ascii();
void do_base(enum base_type);
void do_business();
void do_calc();
void do_calctool(int, char **);
void do_clear();
void do_clear_entry();
void do_delete();
void do_numtype(enum num_type);
void do_expno();
void do_help();
void do_immed();
void do_memory(int);
void do_mode();
void do_none();
void do_number();
void do_paren();
void do_pending();
void do_point();
void do_portion();
void do_trig();
void do_trigtype(enum trig_type);
void doerr(char *);
void get_display();
void get_key_val(int *, char *);
void get_options(int, char **);
void grey_buttons(enum base_type);
void handle_menu_selection(struct button *, int);
void handle_selection();
void initialise();
void init_args();
void init_frame_sizes();
void init_vars();
void init_Xvars(int *, char **);
void key_init();
void load_resources();
void make_frames();
void make_menus();
void make_reg(int, char *);
void make_registers();
void MPstr_to_num(char *, enum base_type, int *);
void paren_disp(char);
void process_event(int);
void process_item(struct button *);
void process_stack(int, int, int);
void process_str(char *);
void push_num(int *);
void push_op(int);
void put_resource(enum res_type, char *);
void read_rcfiles();
void read_resources();
void read_str(char **, char *);
void save_pending_values(struct button *);
void save_resources();
void set_display(char *str);
void set_help_state(int);
void set_hyp_item(int);
void set_ins_key();
void set_item(enum item_type, int);
void set_inv_item(int);
void set_mode(enum mode_type);
void set_op_item(char *);
void set_title(enum fcp_type, char *);
void show_ascii_frame();
void show_display(int *);
void srand48();
void start_tool();
void switch_hands(int);
void usage(char *);
void win_display(enum fcp_type, int);
void write_rcfile(enum menu_type, int, int, char *, char *);
void write_resources();

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
void mpclr(int *, int *);
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
