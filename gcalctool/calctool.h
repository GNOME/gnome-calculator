
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

#define cpNULL  (char *) NULL

#ifdef XGETTEXT
#define MSGFILE_LABEL    "SUNW_DESKSET_CALCTOOL_LABEL"
#else
extern char *MSGFILE_LABEL;
#endif

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

enum base_type { BIN, OCT, DEC, HEX };      /* Base definitions. */

/* Main calctool window types. */
enum fcp_type  { FCP_KEY, FCP_REG, FCP_MODE };

/* Pseudo panel items. */
enum item_type { BASEITEM, DISPLAYITEM, TTYPEITEM, NUMITEM,
                 HYPITEM,  INVITEM,     OPITEM,    MODEITEM };

/* Popup menu types. */
enum menu_type { M_ACC,  M_BASE, M_CON,  M_EXCH, M_FUN,  M_LSHF,  M_MODE,
                 M_NUM,  M_RCL,  M_RSHF, M_STO,  M_TRIG, M_PROPS, M_NONE
};

/* Calculator modes. */
enum mode_type { BASIC, FINANCIAL, LOGICAL, SCIENTIFIC };

enum num_type { ENG, FIX, SCI };            /* Number display mode. */

enum op_type { OP_SET, OP_CLEAR, OP_NOP };  /* Operation item settings. */

/* Resources. */
enum res_type { R_ACCURACY, R_BASE,  R_DISPLAY, R_MODE, R_HELP,
                R_REGS,     R_RHAND, R_TRIG,    R_BEEP
};

enum trig_type { DEG, GRAD, RAD };          /* Trigonometric types. */

/* Abbreviations for the calctool keyboard and menu equivalents. */

#define KEY_D     buttons[4].value              /* d */
#define KEY_E     buttons[5].value              /* e */
#define KEY_F     buttons[6].value              /* f */
#define KEY_CLR   buttons[7].value              /* del */
#define KEY_INT   buttons[2].value              /* CTL('i') */
#define KEY_FRAC  buttons[3].value              /* CTL('f') */
#define KEY_BASE  buttons[1].value              /* B */
#define KEY_DISP  buttons[0].value              /* D */

#define KEY_A     buttons[12].value             /* a */
#define KEY_B     buttons[13].value             /* b */
#define KEY_C     buttons[14].value             /* c */
#define KEY_BSP   buttons[15].value             /* CTL('h') */
#define KEY_ABS   buttons[10].value             /* CTL('u') */
#define KEY_CHS   buttons[11].value             /* C */
#define KEY_KEYS  buttons[9].value              /* k */
#define KEY_MODE  buttons[8].value              /* M */

#define KEY_7     buttons[20].value             /* 7 */
#define KEY_8     buttons[21].value             /* 8 */
#define KEY_9     buttons[22].value             /* 9 */
#define KEY_MUL   buttons[23].value             /* x */
#define KEY_REC   buttons[18].value             /* r */
#define KEY_SQR   buttons[19].value             /* @ */
#define KEY_ACC   buttons[17].value             /* A */
#define KEY_MEM   buttons[16].value             /* m */

#define KEY_4     buttons[28].value             /* 4 */
#define KEY_5     buttons[29].value             /* 5 */
#define KEY_6     buttons[30].value             /* 6 */
#define KEY_DIV   buttons[31].value             /* / */
#define KEY_PER   buttons[26].value             /* % */
#define KEY_SQRT  buttons[27].value             /* s */
#define KEY_CON   buttons[25].value             /* # */
#define KEY_FUN   buttons[24].value             /* F */

#define KEY_1     buttons[36].value             /* 1 */
#define KEY_2     buttons[37].value             /* 2 */
#define KEY_3     buttons[38].value             /* 3 */
#define KEY_SUB   buttons[39].value             /* - */
#define KEY_LPAR  buttons[34].value             /* ( */
#define KEY_RPAR  buttons[35].value             /* ) */
#define KEY_RCL   buttons[33].value             /* R */
#define KEY_STO   buttons[32].value             /* S */

#define KEY_0     buttons[44].value             /* 0 */
#define KEY_PNT   buttons[45].value             /* . */
#define KEY_EQ    buttons[46].value             /* = */
#define KEY_ADD   buttons[47].value             /* + */
#define KEY_EXP   buttons[42].value             /* E */
#define KEY_ASC   buttons[43].value             /* CTL('a') */
#define KEY_EXCH  buttons[41].value             /* X */
#define KEY_QUIT  buttons[40].value             /* q */

#define KEY_CTRM  mode_buttons[0].value         /* CTL('t') */
#define KEY_DDB   mode_buttons[1].value         /* CTL('d') */
#define KEY_FV    mode_buttons[2].value         /* v */
#define KEY_PMT   mode_buttons[3].value         /* P */
#define KEY_PV    mode_buttons[4].value         /* p */
#define KEY_RATE  mode_buttons[5].value         /* CTL('r') */
#define KEY_SLN   mode_buttons[6].value         /* CTL('s') */
#define KEY_SYD   mode_buttons[7].value         /* CTL('y') */
#define KEY_TERM  mode_buttons[8].value         /* T */

#define KEY_LSFT  mode_buttons[16].value        /* < */
#define KEY_RSFT  mode_buttons[17].value        /* > */
#define KEY_16    mode_buttons[18].value        /* [ */
#define KEY_32    mode_buttons[19].value        /* ] */
#define KEY_OR    mode_buttons[24].value        /* | */
#define KEY_AND   mode_buttons[25].value        /* & */
#define KEY_NOT   mode_buttons[26].value        /* ~ */
#define KEY_XOR   mode_buttons[27].value        /* ^ */
#define KEY_XNOR  mode_buttons[28].value        /* n */

#define KEY_TRIG  mode_buttons[32].value        /* T */
#define KEY_HYP   mode_buttons[33].value        /* h */
#define KEY_INV   mode_buttons[34].value        /* i */
#define KEY_ETOX  mode_buttons[35].value        /* { */
#define KEY_TTOX  mode_buttons[36].value        /* } */
#define KEY_YTOX  mode_buttons[37].value        /* y */
#define KEY_FACT  mode_buttons[38].value        /* ! */
#define KEY_COS   mode_buttons[40].value        /* CTL('c') */
#define KEY_SIN   mode_buttons[41].value        /* CTL('s') */
#define KEY_TAN   mode_buttons[42].value        /* CTL('t') */
#define KEY_LN    mode_buttons[43].value        /* N */
#define KEY_LOG   mode_buttons[44].value        /* G */
#define KEY_RAND  mode_buttons[45].value        /* ? */

#define BCOLS          8          /* No of columns of buttons. */
#define BROWS          6          /* No of rows of buttons. */
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
#define MAXMENUS       13         /* Maximum number of popup menus. */

#ifndef MAXLINE
#define MAXLINE        256        /* Length of character strings. */
#endif /*MAXLINE*/

#define MAXACC         10    /* Max. number of digits+1 after numeric point. */
#define MAXBASES       4          /* Maximum number of numeric bases. */
#define MAXCONFUN      10         /* Maximum number of constants/functions. */
#define MAXDISPMODES   3          /* Maximum number of display modes. */
#define MAXENTRIES     50         /* Maximum number of menu entries. */
#define MAXMODES       4          /* Maximum number of calculator modes. */
#define MAXREGS        10         /* Maximum number of memory registers. */
#define MAXSTACK       256        /* Parenthese stack size. */
#define MAXTRIGMODES   3          /* Maximum number of trig. modes. */
#define MAXVKEYS       5          /* Number of valid keys after an error. */

#define MCOLS          8          /* Number of columns of "special" keys. */
#define MROWS          2          /* Number of rows of "special" keys. */
#define MODEKEYS       MCOLS * MROWS

#define MAXCOLS        ((v->curwin == FCP_KEY) ? BCOLS : MCOLS)
#define MAXROWS        ((v->curwin == FCP_KEY) ? BROWS : MROWS)

#ifndef MIN
#define MIN(x,y)       ((x) < (y) ? (x) : (y))
#endif /*MIN*/

#define NOBUTTONS      BROWS * BCOLS

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
    char item_text[MAXITEMS][60];      /* Pseudo panel item text strings. */
    char old_cal_value;                /* Previous calculation operator. */
    char opstr[5];                     /* Operand string during pending op. */
    char *progname;                    /* Name of this program. */
    char pstr[5];                      /* Current button text string. */
    char *shelf;                       /* PUT selection shelf contents. */
    char snum[MAX_DIGITS+1];           /* Scratchpad for scientific numbers. */
    char *titleline;                   /* Value of titleline (if present). */

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
    enum fcp_type pending_win;      /* Window that pending op came from. */
    enum mode_type modetype;        /* Current calculator mode. */
    enum mode_type pending_mode;    /* Mode for pending op. */
    enum num_type dtype;            /* Number display mode. */
    enum trig_type ttype;           /* Trig. type (deg, grad or rad). */

    int accuracy;      /* Number of digits precision (Max 9). */
    int beep;          /* Indicates whether there is a beep sound on error. */
    int cur_ch;        /* Current character if keyboard event. */
    int current;       /* Current button/character pressed. */
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
    int pending_n;     /* Offset into function table for pending op. */
    int pending_op;    /* Arithmetic operation for pending command. */
    int pointed;       /* Whether a decimal point has been given. */
    int righthand;     /* Set if this is a "right-handed" calculator. */
    int rstate;        /* Indicates if memory register frame is displayed. */
    int show_help;     /* Set if we wish to show tooltip help. */
    int show_paren;    /* Set if we wish to show DISPLAYITEM during parens. */
    int started;       /* Set just before window is displayed. */
    int toclear;       /* Indicates if display should be cleared. */
    int tstate;        /* Indicates current button set being displayed. */
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

char *button_str(int);
char *convert(char *);
char *get_resource(enum res_type);
char *make_number(int *, BOOLEAN);

enum menu_type button_mtype(int);
enum op_type button_opdisp(int);

unsigned short *get_but_data();

int button_mods(int);
int button_value(int);
int char_val(char);
int get_menu_entry(enum menu_type, int);
int main(int, char **);

void beep();
void clear_display();
void display_prop_sheet();
void do_ascii();
void do_business();
void do_calc();
void do_calctool(int, char **);
void do_clear();
void do_delete();
void do_expno();
void do_frame();
void do_help();
void do_immed();
void do_keys();
void do_memory();
void do_none();
void do_number();
void do_paren();
void do_pending();
void do_point();
void do_portion();
void do_trig();
void doerr(char *);
void get_display();
void get_key_val(int *, char *);
void get_label(int);
void get_options(int, char **);
void grey_buttons(enum base_type);
void handle_menu_selection(int, int);
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
void process_item(int);
void process_stack(int, int, int);
void process_str(char *);
void push_num(int *);
void push_op(int);
void put_resource(enum res_type, char *);
void read_rcfiles();
void read_resources();
void read_str(char **, char *);
void save_pending_values(int);
void save_resources();
void set_button_state(enum fcp_type, int, int);
void set_help_state(int);
void set_ins_key();
void set_item(enum item_type, char *);
void set_label(enum item_type itemno, char *str);
void set_mode(enum mode_type);
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
