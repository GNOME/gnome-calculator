
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
#include "color.h"
#include "calctool.h"

time_t time();

static void init_text();

double max_fix[4] = {
    6.871947674e+10, 3.245185537e+32,
    1.000000000e+36, 2.230074520e+43
};

char *base_str[]  = {          /* Strings for each base value. */
    _("BIN"), _("OCT"), _("DEC"), _("HEX")
};

char *calc_res[] = {
    "accuracy",            "base",            "display",        "mode",
    "showRegisters",       "rightHanded",     "3dLook",         "hasTitle",
    "trigType",            "decDigitColor",   "hexDigitColor",  "arithOpColor",
    "adjustColor",         "portionColor",    "functionColor",  "mainModeColor",
    "portionLogicalColor", "bitLogicalColor", "finColor",       "trigModeColor",
    "trigColor",           "sciColor",       "backgroundColor", "displayColor",
    "memRegisterColor",    "textColor",       "buttonFont",     "modeFont",
    "memoryFont",          "displayFont",     "beep"
};

char *dtype_str[] = {          /* Strings for each display mode value. */
    _("ENG"), _("FIX"), _("SCI")
};

char *mode_str[]  = {          /* Strings for each mode value. */
    _("BASIC"), _("FINANCIAL"), _("LOGICAL"), _("SCIENTIFIC")
};

char *mstrs[] = {              /* Mode titles for the popup panel. */
    _("Basic Mode."), _("Financial Mode."),
    _("Logical Mode."), _("Scientific Mode.")
};


char *ttype_str[] = {          /* Strings for each trig type value. */
    _("DEG"), _("GRAD"), _("RAD")
};

char digits[] = "0123456789ABCDEF";
int basevals[4] = { 2, 8, 10, 16 };

int left_pos[BCOLS]  = { 7, 6, 4, 5, 0, 1, 2, 3 };  /* Left positions. */
int right_pos[BCOLS] = { 4, 5, 6, 7, 2, 3, 1, 0 };  /* "Right" positions. */

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

/* Calculator button values. */

struct button buttons[NOBUTTONS] = {
/* str        hstr    mods              value           opdisp   menutype   color       func */

/* Row 1. */
{ _("D   "),  "D   ", 0,                GDK_d,          OP_NOP,   M_NONE, C_HEXDIG,   do_number    },
{ _("E   "),  "E   ", 0,                GDK_e,          OP_NOP,   M_NONE, C_HEXDIG,   do_number    },
{ _("F   "),  "F   ", 0,                GDK_f,          OP_NOP,   M_NONE, C_HEXDIG,   do_number    },
{ _("Clr "),  "Clr ", 0,                GDK_Delete,     OP_CLEAR, M_NONE, C_ADJUST,   do_clear     },
{ _("Int "),  "Int ", GDK_CONTROL_MASK, GDK_i,          OP_CLEAR, M_NONE, C_PORTION,  do_portion   },
{ _("Frac"),  "Frac", GDK_CONTROL_MASK, GDK_f,          OP_CLEAR, M_NONE, C_PORTION,  do_portion   },
{ _("Base"),  "Base", GDK_SHIFT_MASK,   GDK_B,          OP_SET,   M_BASE, C_MAINMODE, do_pending   },
{ _("Disp"),  "Disp", GDK_SHIFT_MASK,   GDK_D,          OP_SET,   M_NUM,  C_MAINMODE, do_pending   },

/* Row 2. */
{ _("A   "),  "A   ", 0,                GDK_a,          OP_NOP,   M_NONE, C_HEXDIG,   do_number    },
{ _("B   "),  "B   ", 0,                GDK_b,          OP_NOP,   M_NONE, C_HEXDIG,   do_number    },
{ _("C   "),  "C   ", 0,                GDK_c,          OP_NOP,   M_NONE, C_HEXDIG,   do_number    },
{ _("Bsp "),  "Bsp ", 0,                GDK_BackSpace,  OP_NOP,   M_NONE, C_ADJUST,   do_delete    },
{ _("Abs "),  "Abs ", GDK_CONTROL_MASK, GDK_u,          OP_CLEAR, M_NONE, C_PORTION,  do_portion   },
{ _("+/- "),  "+/- ", GDK_SHIFT_MASK,   GDK_C,          OP_CLEAR, M_NONE, C_PORTION,  do_immed     },
{ _("Keys"),  "Keys", 0,                GDK_k,          OP_CLEAR, M_NONE, C_FUNC,     do_keys      },
{ _("Mode"),  "Mode", GDK_SHIFT_MASK,   GDK_M,          OP_SET,   M_MODE, C_MAINMODE, do_pending   },

/* Row 3. */
{ _("7   "),  "7   ", 0,                GDK_7,          OP_NOP,   M_NONE, C_DECDIG,   do_number    },
{ _("8   "),  "8   ", 0,                GDK_8,          OP_NOP,   M_NONE, C_DECDIG,   do_number    },
{ _("9   "),  "9   ", 0,                GDK_9,          OP_NOP,   M_NONE, C_DECDIG,   do_number    },
{ _("X   "),  "X   ", 0,                GDK_x,          OP_SET,   M_NONE, C_ARITHOP,  do_calc      },
{ _("1/x "),  "1/x ", 0,                GDK_r,          OP_CLEAR, M_NONE, C_ARITHOP,  do_immed     },
{ _("x^2 "),  "x^2 ", GDK_SHIFT_MASK,   GDK_at,         OP_CLEAR, M_NONE, C_ARITHOP,  do_immed     },
{ _("Acc "),  "Acc ", GDK_SHIFT_MASK,   GDK_A,          OP_SET,   M_ACC,  C_FUNC,     do_pending   },
{ _("Mem..."), "Mem...", 0,             GDK_m,          OP_NOP,   M_NONE, C_FUNC,     do_memory    },

/* Row 4. */
{ _("4   "),  "4   ", 0,                GDK_4,          OP_NOP,   M_NONE, C_DECDIG,   do_number    },
{ _("5   "),  "5   ", 0,                GDK_5,          OP_NOP,   M_NONE, C_DECDIG,   do_number    },
{ _("6   "),  "6   ", 0,                GDK_6,          OP_NOP,   M_NONE, C_DECDIG,   do_number    },
{ _("/   "),  "/   ", 0,                GDK_slash,      OP_SET,   M_NONE, C_ARITHOP,  do_calc      },
{ _("%   "),  "%   ", GDK_SHIFT_MASK,   GDK_percent,    OP_SET,   M_NONE, C_ARITHOP,  do_calc      },
{ _("Sqrt"),  "Sqrt", 0,                GDK_s,          OP_CLEAR, M_NONE, C_ARITHOP,  do_immed     },
{ _("Con "),  "Con ", GDK_SHIFT_MASK,   GDK_numbersign, OP_SET,   M_CON,  C_FUNC,     do_pending   },
{ _("Fun "),  "Fun ", GDK_SHIFT_MASK,   GDK_F,          OP_SET,   M_FUN,  C_FUNC,     do_pending   },

/* Row 5. */
{ _("1   "),  "1   ", 0,                GDK_1,          OP_NOP,   M_NONE, C_DECDIG,   do_number    },
{ _("2   "),  "2   ", 0,                GDK_2,          OP_NOP,   M_NONE, C_DECDIG,   do_number    },
{ _("3   "),  "3   ", 0,                GDK_3,          OP_NOP,   M_NONE, C_DECDIG,   do_number    },
{ _("-   "),  "-   ", 0,                GDK_minus,      OP_SET,   M_NONE, C_ARITHOP,  do_calc      },
{ _("(   "),  "(   ", GDK_SHIFT_MASK,   GDK_parenleft,  OP_SET,   M_NONE, C_ARITHOP,  do_paren     },
{ _(")   "),  ")   ", GDK_SHIFT_MASK,   GDK_parenright, OP_SET,   M_NONE, C_ARITHOP,  do_paren     },
{ _("Rcl "),  "Rcl ", GDK_SHIFT_MASK,   GDK_R,          OP_SET,   M_RCL,  C_FUNC,     do_pending   },
{ _("Sto "),  "Sto ", GDK_SHIFT_MASK,   GDK_S,          OP_SET,   M_STO,  C_FUNC,     do_pending   },

/* Row 6. */
{ _("0   "),  "0   ", 0,                GDK_0,          OP_NOP,   M_NONE, C_DECDIG,   do_number    },
{ _(".   "),  ".   ", 0,                GDK_period,     OP_NOP,   M_NONE, C_DECDIG,   do_point     },
{ _("=   "),  "=   ", 0,                GDK_equal,      OP_CLEAR, M_NONE, C_ARITHOP,  do_calc      },
{ _("+   "),  "+   ", GDK_SHIFT_MASK,   GDK_plus,       OP_SET,   M_NONE, C_ARITHOP,  do_calc      },
{ _("Exp "),  "Exp ", GDK_SHIFT_MASK,   GDK_E,          OP_SET,   M_NONE, C_ARITHOP,  do_expno     },
{ _("Asc..."), "Asc...", GDK_CONTROL_MASK, GDK_a,       OP_CLEAR, M_NONE, C_ARITHOP,  do_ascii     },
{ _("Exch"),  "Exch", GDK_SHIFT_MASK,   GDK_X,          OP_SET,   M_EXCH, C_FUNC,     do_pending   },
{ _("Quit"),  "Quit", 0,                GDK_q,          OP_CLEAR, M_NONE, C_FUNC,     do_frame     },
};

struct button mode_buttons[(MAXMODES-1) * MODEKEYS] = {
/* str        hstr    mods              value           opdisp   menutype color       func */

/* Financial. */
{ _("Ctrm"),  "Ctrm", GDK_CONTROL_MASK, GDK_m,          OP_CLEAR, M_NONE, C_FIN,      do_business  },
{ _("Ddb "),  "Ddb ", GDK_CONTROL_MASK, GDK_d,          OP_CLEAR, M_NONE, C_FIN,      do_business  },
{ _("Fv  "),  "Fv  ", 0,                GDK_v,          OP_CLEAR, M_NONE, C_FIN,      do_business  },
{ _("Pmt "),  "Pmt ", GDK_SHIFT_MASK,   GDK_P,          OP_CLEAR, M_NONE, C_FIN,      do_business  },
{ _("Pv  "),  "Pv  ", 0,                GDK_p,          OP_CLEAR, M_NONE, C_FIN,      do_business  },
{ _("Rate"),  "Rate", GDK_CONTROL_MASK, GDK_r,          OP_CLEAR, M_NONE, C_FIN,      do_business  },
{ _("Sln "),  "Sln ", GDK_CONTROL_MASK, GDK_l,          OP_CLEAR, M_NONE, C_FIN,      do_business  },
{ _("Syd "),  "Syd ", GDK_CONTROL_MASK, GDK_y,          OP_CLEAR, M_NONE, C_FIN,      do_business  },
{ _("Term"),  "Term", 0,                GDK_T,          OP_CLEAR, M_NONE, C_FIN,      do_business  },
{ "    ",     "    ", 0,                0,              OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,                0,              OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,                0,              OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,                0,              OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,                0,              OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,                0,              OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,                0,              OP_NOP,   M_NONE, C_BACK,     do_none      },

/* Logical. */
{ _(" <  "),  " <  ", GDK_SHIFT_MASK, GDK_less,         OP_SET,   M_LSHF, C_PLOGICAL, do_pending   },
{ _(" >  "),  " >  ", GDK_SHIFT_MASK, GDK_greater,      OP_SET,   M_RSHF, C_PLOGICAL, do_pending   },
{ _("&16 "),  "&16 ", 0,              GDK_bracketleft,  OP_CLEAR, M_NONE, C_PLOGICAL, do_immed     },
{ _("&32 "),  "&32 ", 0,              GDK_bracketright, OP_CLEAR, M_NONE, C_PLOGICAL, do_immed     },
{ "    ",     "    ", 0,              0,                OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,              0,                OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,              0,                OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,              0,                OP_NOP,   M_NONE, C_BACK,     do_none      },
{ _("Or  "),  "Or  ", GDK_SHIFT_MASK, GDK_bar,          OP_SET,   M_NONE, C_BLOGICAL, do_calc      },
{ _("And "),  "And ", GDK_SHIFT_MASK, GDK_ampersand,    OP_SET,   M_NONE, C_BLOGICAL, do_calc      },
{ _("Not "),  "Not ", GDK_SHIFT_MASK, GDK_asciitilde,   OP_CLEAR, M_NONE, C_BLOGICAL, do_immed     },
{ _("Xor "),  "Xor ", GDK_SHIFT_MASK, GDK_caret,        OP_SET,   M_NONE, C_BLOGICAL, do_calc      },
{ _("Xnor"),  "Xnor", 0,              GDK_n,            OP_SET,   M_NONE, C_BLOGICAL, do_calc      },
{ "    ",     "    ", 0,              0,                OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,              0,                OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,              0,                OP_NOP,   M_NONE, C_BACK,     do_none      },

/* Scientific. */
{ _("Trig"),  "Trig", GDK_SHIFT_MASK,   GDK_T,          OP_SET,   M_TRIG, C_TRIGMODE, do_pending   },
{ _("Hyp "),  "Hyp ", 0,                GDK_h,          OP_CLEAR, M_NONE, C_TRIGMODE, do_immed     },
{ _("Inv "),  "Inv ", 0,                GDK_i,          OP_CLEAR, M_NONE, C_TRIGMODE, do_immed     },
{ _("e^x "),  "e^x ", GDK_SHIFT_MASK,   GDK_braceleft,  OP_CLEAR, M_NONE, C_SCI,      do_immed     },
{ _("10^x"),  "10^x", GDK_SHIFT_MASK,   GDK_braceright, OP_CLEAR, M_NONE, C_SCI,      do_immed     },
{ _("y^x "),  "y^x ", 0,                GDK_y,          OP_SET,   M_NONE, C_SCI,      do_calc      },
{ _("x!  "),  "x!  ", GDK_SHIFT_MASK,   GDK_exclam,     OP_CLEAR, M_NONE, C_SCI,      do_immed     },
{ "    ",     "    ", 0,                0,              OP_NOP,   M_NONE, C_BACK,     do_none      },
{ _("Cos "),  "Cos ", GDK_CONTROL_MASK, GDK_c,          OP_CLEAR, M_NONE, C_TRIGCOL,  do_trig      },
{ _("Sin "),  "Sin ", GDK_CONTROL_MASK, GDK_s,          OP_CLEAR, M_NONE, C_TRIGCOL,  do_trig      },
{ _("Tan "),  "Tan ", GDK_CONTROL_MASK, GDK_t,          OP_CLEAR, M_NONE, C_TRIGCOL,  do_trig      },
{ _("Ln  "),  "Ln  ", GDK_SHIFT_MASK,   GDK_N,          OP_CLEAR, M_NONE, C_SCI,      do_immed     },
{ _("Log "),  "Log ", GDK_SHIFT_MASK,   GDK_G,          OP_CLEAR, M_NONE, C_SCI,      do_immed     },
{ _("Rand"),  "Rand", GDK_SHIFT_MASK,   GDK_question,   OP_CLEAR, M_NONE, C_SCI,      do_immed     },
{ "    ",     "    ", 0,                0,              OP_NOP,   M_NONE, C_BACK,     do_none      },
{ "    ",     "    ", 0,                0,           OP_NOP,   M_NONE, C_BACK,     do_none      },
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

    calc_colorsetup(v->rcols, v->gcols, v->bcols);  /* Setup default colors. */

    init_text();               /* Setup text strings depending upon language. */
    init_vars();               /* Setup default values for variables. */
    load_resources();          /* Get resources from various places. */
    read_resources();          /* Read resources from merged database. */
    get_options(argc, argv);   /* Get command line arguments. */
    read_rcfiles();            /* Read .calctoolrc's files. */

    if (v->righthand) {              /* Display a right-handed calculator. */
        switch_hands(v->righthand);
    }
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
matherr(struct exception *exc)
{
    doerr(_("Error"));

    return(1);
}
