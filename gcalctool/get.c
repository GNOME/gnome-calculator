
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
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/param.h>
#include "calctool.h"
#include "extern.h"
#include "config.h"

static char *set_bool(int);

static int get_bool_resource(enum res_type, int *);
static int get_int_resource(enum res_type, int *);
static int get_str_resource(enum res_type, char *);

static void getparam(char *, char **, char *);
static void get_rcfile(char *);


char *
convert(char *line)       /* Convert .gcalctoolcf line to ascii values. */
{
    static char output[MAXLINE];   /* Converted output record. */
    int ctrl = 0;           /* Set if we are processing a control character. */
    int i;                  /* Position within input line. */
    int len;
    int n = 0;              /* Position within output line. */

    len = strlen(line);
    for (i = 0; i < len; i++) {
        if (line[i] == ' ') {
            continue;
        } else if (line[i] == '\\') {
            ctrl = 1;
        } else if (ctrl) {
            output[n++] = CTL(line[i]);
            ctrl = 0;
        } else {
            output[n++] = line[i];
        }
    }
    output[n] = '\0';

    return(output);
}


/* Get boolean resource from database. */

static int
get_bool_resource(enum res_type rtype, int *boolval)
{
    char *val, tempstr[MAXLINE];
    int len, n;

    if ((val = get_resource(rtype)) == NULL) {
        return(0);
    }
    STRCPY(tempstr, val);
    len = strlen(tempstr);
    for (n = 0; n < len; n++) {
        if (isupper((int) tempstr[n])) {
            tempstr[n] = tolower((int) tempstr[n]);
        }
    }
    if (EQUAL(tempstr, "true")) {
        *boolval = TRUE;
    } else {
        *boolval = FALSE;
    }

    return(1);
}


/* Get integer resource from database. */

static int
get_int_resource(enum res_type rtype, int *intval)
{
    char *val;
 
    if ((val = get_resource(rtype)) == NULL) {
        return(0);
    }
    *intval = atoi(val);

    return(1);
}


/* Get keyboard equivalent from first character of localised string. */

void
get_key_val(int *val, char *str)
{
    *val = str[0];
}


void
get_options(int argc, char *argv[])      /* Extract command line options. */
{
    char next[MAXLINE];       /* The next command line parameter. */

    INC;
    while (argc > 0) {
        if (argv[0][0] == '-') {
            switch (argv[0][1]) {
                case 'D' :                   /* MP debug info. to stderr. */
                    v->MPdebug = TRUE;
                    break;

                case 'E' :                   /* MP errors to stderr. */
                    v->MPerrors = TRUE;
                    break;

                case 'a' : 
                    INC;
                    getparam(next, argv, _("-a needs accuracy value"));
                    v->accuracy = atoi(next);
                    if (v->accuracy < 0 || v->accuracy > 9) {
                        FPRINTF(stderr, 
                                _("%s: accuracy should be in the range 0-9\n"),
                                v->progname);
                        v->accuracy = 2;
                    }
                    break;

                case 'l' :                    /* "Left-handed" version. */
                    v->righthand = 0;
                    break;

                case 'r' :                    /* "Right-handed" version */
                    v->righthand = 1;
                    break;

                case '?' :
                case 'v' : 
                    usage(v->progname);
                    break;

                default :
                    usage(v->progname);
            }
            INC;
        } else {
            INC;
        }
    }
}


static void
getparam(char *s, char *argv[], char *errmes)
{
    if (*argv != NULL && argv[0][0] != '-') {
        STRCPY(s, *argv);
    } else { 
        FPRINTF(stderr, _("%s: %s as next argument.\n"), v->progname, errmes);
        exit(1);                        
    }                                  
}


static void
get_rcfile(char *name)          /* Read .gcalctoolcf file. */
{
    char line[MAXLINE];    /* Current line from the .gcalctoolcf file. */
    char tmp[MAXLINE];     /* Used to extract definitions. */
    double cval;           /* Current constant value being converted. */
    enum base_type base;   /* Saved current base value. */
    int i;                 /* Index to constant or function array. */
    int isval;             /* Set to 'c' or 'f' for convertable line. */
    int len, n;
    FILE *rcfd;            /* File descriptor for calctool rc file. */

    if ((rcfd = fopen(name, "r")) == NULL) {
        return;
    }

/*  Process the .gcalctoolcf file. There are currently four types of
 *  records to look for:
 *
 *  1) Those starting with a hash in the first column are comments.
 *
 *  2) Lines starting with 'c' or 'C' in the first column are
 *     definitions for constants. The cC is followed by a digit in
 *     the range 0-9, then a space. This is followed by a number
 *     in fixed or scientific notation. Following this is an optional
 *     comment, which if found, will be used in the popup menu for
 *     the constants. If the comment is present, there must be at
 *     least one space between this and the preceding number.
 *
 *  3) Those starting with a 'f' or a 'F' in the first column are
 *     definitions for functions. The fF is followed by a digit in
 *     the range 0-9, then a space. This is followed by a function
 *     definition. Following this is an optional comment, which if
 *     found, will be used in the popup menu for the functions.
 *     If the comment is present, there must be at least one space
 *     between this and the preceding function definition.
 *
 *  4) Lines starting with a 'r' or a 'R' in the first column are
 *     definitions for the initial contents of the calculators
 *     memory registers. The rR is followed by a digit in the
 *     range 0-9, then a space. This is followed by a number in
 *     fixed or scientific notation. The rest of the line is ignored.
 *
 *  All other lines are ignored.
 *
 *  Two other things to note. There should be no embedded spaces in
 *  the function definitions, and whenever a backslash is found, that
 *  and the following character signify a control character, for
 *  example \g would be ascii 7.
 */

    while (fgets(line, MAXLINE, rcfd) != NULL) {
        isval = 0;
        if (line[0] == 'c' || line[0] == 'C') {
            isval = 'c';
        } else if (line[0] == 'f' || line[0] == 'F') {
            isval = 'f';
        } else if (line[0] == 'r' || line[0] == 'R') {
            isval = 'r';
        }
        if (isval) {
            if (line[1] >= '0' && line[1] <= '9' && line[2] == ' ') {
                i = char_val(line[1]);
                if (isval == 'c') {
                    n = sscanf(&line[3], "%lf", &cval);
                    if (n == 1) {
                        MPstr_to_num(&line[3], DEC, v->MPcon_vals[i]);
                    }
                } else if (isval == 'f') {
                    SSCANF(&line[3], "%s", tmp);      
                    STRCPY(v->fun_vals[i], convert(tmp));
                } else if (isval == 'r') {
                    n = sscanf(&line[3], "%lf", &cval);
                    if (n == 1) MPstr_to_num(&line[3], DEC, v->MPmvals[i]);
                    continue;
                }
                len = strlen(line);
                for (n = 3; n < len; n++) {
                    if (line[n] == ' ' || line[n] == '\n') {
                        while (line[n] == ' ') {
                            n++;
                        }
                        line[strlen(line)-1] = '\0';
                        if (isval == 'c') {
                            base = v->base;
                            v->base = DEC;
                            STRCPY(tmp, make_number(v->MPcon_vals[i]));
                            v->base = base;
                            SPRINTF(v->con_names[i], "%1d: %s [%s]",
                                    i, tmp, &line[n]);
                        } else {
                            SPRINTF(v->fun_names[i], "%1d: %s [%s]",
                                    i, tmp, &line[n]);
                        }
                        break;
                    }
                }
            }
        }
    }
    FCLOSE(rcfd);
}


/* Get a string resource from database. */

static int
get_str_resource(enum res_type rtype, char *strval)
{
    char *val;
    int i, len;

    if ((val = get_resource(rtype)) == NULL) {
        return(0);
    }
    STRCPY(strval, val);
    len = strlen(strval);
    for (i = 0; i < len; i++) {
        if (islower((int) strval[i])) {
            strval[i] = toupper((int) strval[i]);
        }
    }

    return(1);
}


void
init_vars()    /* Setup default values for various variables. */
{
    int acc, i, n, size;

    v->righthand     = 1;      /* "Right-handed" calculator by default. */
    v->accuracy      = 2;      /* Initial accuracy. */
    v->base          = DEC;    /* Initial base. */
    v->dtype         = FIX;    /* Initial number display mode. */
    v->ttype         = DEG;    /* Initial trigonometric type. */
    v->modetype      = BASIC;  /* Initial calculator mode. */
    v->rstate        = 0;      /* No memory register frame display initially. */
    v->iconic        = FALSE;  /* Calctool not iconic by default. */
    v->MPdebug       = FALSE;  /* No debug info by default. */
    v->MPerrors      = FALSE;               /* No error information. */
    acc              = MAX_DIGITS + 12;     /* MP internal accuracy. */
    size             = MP_SIZE;
    mpset(&acc, &size, &size);

    v->iheight = v->iwidth = -1;   /* To signify no initial size. */
    v->hasicon     = FALSE;        /* Use standard calctool icon by default. */
    v->beep        = TRUE;         /* Beep on error by default. */
    v->error       = 0;            /* No calculator error initially. */
    v->key_exp     = 0;            /* Not entering an exponent number. */
    v->pending_op  = 0;            /* No pending arithmetic operation. */
    v->titleline   = NULL;         /* No User supplied title line. */

    read_str(&v->iconlabel, _("calculator"));  /* Default icon label. */

    MPstr_to_num("0.621", DEC, v->MPcon_vals[0]);  /* kms/hr <=> miles/hr. */
    MPstr_to_num("1.4142135623", DEC, v->MPcon_vals[1]);  /* square root of 2 */
    MPstr_to_num("2.7182818284", DEC, v->MPcon_vals[2]);  /* e */
    MPstr_to_num("3.1415926535", DEC, v->MPcon_vals[3]);  /* pi */
    MPstr_to_num("2.54",         DEC, v->MPcon_vals[4]);  /* cms <=> inch. */
    MPstr_to_num("57.295779513", DEC, v->MPcon_vals[5]);  /* degrees/radian. */
    MPstr_to_num("1048576.0",    DEC, v->MPcon_vals[6]);  /* 2 ^ 20. */
    MPstr_to_num("0.0353", DEC, v->MPcon_vals[7]);  /* grams <=> ounce. */
    MPstr_to_num("0.948",  DEC, v->MPcon_vals[8]);  /* Kjoules <=> BTU's. */
    MPstr_to_num("0.0610", DEC, v->MPcon_vals[9]);  /* cms3 <=> inches3. */

    for (i = 0; i < MAXITEMS; i++) {
        v->item_text[i][0] = 0;
    }

    n = 0;
    for (i = 0; i < MAXREGS; i++) {
        mpcim(&n, v->MPmvals[i]);
    }
}


void
read_rcfiles()   /* Read .gcalctoolcf's from home and current directories. */
{
    char name[MAXLINE];          /* Full name of users .gcalctoolcf file. */
    char pathname[MAXPATHLEN];   /* Current working directory. */
    char tmp[MAXLINE];           /* For temporary constant string creation. */
    int n;

    for (n = 0; n < MAXREGS; n++) {
        STRCPY(tmp, make_number(v->MPcon_vals[n]));
        SPRINTF(name, "%1d: %s [%s]", n, tmp, v->con_names[n]);

        STRCPY(v->con_names[n], name);
        STRCPY(v->fun_vals[n], "");    /* Initially empty function strings. */
    }

    SPRINTF(name, "%s/%s", v->home, CFNAME);
    get_rcfile(name);      /* Read .gcalctoolcf from users home directory. */

    SPRINTF(name, "%s/%s", getcwd(pathname, MAXPATHLEN+1), CFNAME);
    get_rcfile(name);      /* Read .gcalctoolcf file from current directory. */
}


void
read_resources()    /* Read all possible resources from the database. */
{
    int boolval, i, intval;
    char str[MAXLINE];

    if (get_int_resource(R_ACCURACY, &intval)) {
        v->accuracy = intval;
        if (v->accuracy < 0 || v->accuracy > 9) {
            FPRINTF(stderr, _("%s: accuracy should be in the range 0-9\n"), 
                    v->progname);
            v->accuracy = 2;
        }
    }

    if (get_str_resource(R_BASE, str)) {
        for (i = 0; i < MAXBASES; i++) {
            if (EQUAL(str, Rbstr[i])) {
                break;
            }
        }

        if (i == MAXBASES) {
            FPRINTF(stderr, _("%s: base should be 2, 8, 10 or 16\n"), 
                    v->progname);
        } else {
            v->base = (enum base_type) i;
        }
    }

    if (get_str_resource(R_DISPLAY, str)) {
        for (i = 0; i < MAXDISPMODES; i++) {
            if (EQUAL(str, Rdstr[i])) {
                break;
            }
        }

        if (i == MAXDISPMODES) {
            FPRINTF(stderr, _("%s: invalid display mode [%s]\n"), 
                    v->progname, str);
        } else {
            v->dtype = (enum num_type) i;
        }
    }

    if (get_str_resource(R_MODE, str)) {
        for (i = 0; i < MAXMODES; i++) {
            if (EQUAL(str, Rmstr[i])) {
                break;
            }
        }

        if (i == MAXMODES) {
            FPRINTF(stderr, _("%s: invalid mode [%s]\n"), v->progname, str);
        } else {
            v->modetype = (enum mode_type) i;
        }
    }

    if (get_str_resource(R_TRIG, str)) {  
        for (i = 0; i < MAXTRIGMODES; i++) {
            if (EQUAL(str, Rtstr[i])) {
                break;
            }
        }
       
        if (i == MAXTRIGMODES) {
            FPRINTF(stderr, _("%s: invalid trig. mode [%s]\n"), 
                    v->progname, str);
        } else {
            v->ttype = (enum trig_type) i;
        }
    }

    if (get_bool_resource(R_BEEP, &boolval)) {
        v->beep = boolval;
    }
    if (get_bool_resource(R_REGS, &boolval)) {
        v->rstate = boolval;
    }
    if (get_bool_resource(R_RHAND, &boolval)) {
        v->righthand = boolval;
    }
}


void
read_str(char **str, char *value)
{
    if (*str != NULL) {
        (void) free(*str);
    }
    if (value != NULL && strlen(value)) {
        *str = (char *) malloc((unsigned) (strlen(value) + 1));
        STRCPY(*str, value);
    } else {
        *str = NULL;
    }
}


static char *
set_bool(int value)
{
    return(value ? "true" : "false");
}


void
usage(char *progname)
{
    FPRINTF(stderr, _("%s version %s\n\n"), progname, VERSION);
    FPRINTF(stderr, _("Usage: %s: [-D] [-E] [-a accuracy] [-l]\n"), progname);
    FPRINTF(stderr, 
            _("\t\t [-r] [-?] [-v] [-?]\n"));
    exit(1);
}


void
write_rcfile(enum menu_type mtype, int exists, int cfno, 
             char *val, char *comment)
{
    char pathname[MAXPATHLEN];   /* Current working directory. */
    char rcname[MAXPATHLEN];     /* Full name of users .gcalctoolcf file. */
    char str[MAXLINE];           /* Temporary buffer. */
    char sval[3];                /* Used for string comparisons. */
    char tmp_filename[MAXLINE];  /* Used to construct temp filename. */
    int rcexists;                /* Set to 1, if .gcalctoolcf file exists. */
    FILE *rcfd;                  /* File descriptor for .gcalctoolcf file. */
    FILE *tmpfd;                 /* File descriptor for new temp .gcalctoolcf */

    rcexists = 0;
    SPRINTF(rcname, "%s/%s", getcwd(pathname, MAXPATHLEN+1), CFNAME);
    if (access(rcname, F_OK) == 0) {
        rcexists = 1;
    } else { 
        SPRINTF(rcname, "%s/%s", v->home, CFNAME);
        if (access(rcname, F_OK) == 0) {
            rcexists = 1;
        }
    }
    STRCPY(tmp_filename, "/tmp/.gcalctoolcfXXXXXX");
    MKTEMP(tmp_filename);
    if ((tmpfd = fopen(tmp_filename, "w+")) == NULL) {
        return;
    }

    if (rcexists) {
        rcfd = fopen(rcname, "r");
        SPRINTF(sval, " %1d", cfno);
        while (fgets(str, MAXLINE, rcfd)) {
            if (exists) {
                switch (mtype) {
                    case M_CON : 
                        sval[0] = 'c';
                        if (!strncmp(str, sval, 2)) {
                            FPUTS("#", tmpfd);
                        }
                        sval[0] = 'C';
                        if (!strncmp(str, sval, 2)) {
                            FPUTS("#", tmpfd);
                        }
                        break;

                    case M_FUN : 
                        sval[0] = 'f';
                        if (!strncmp(str, sval, 2)) {
                            FPUTS("#", tmpfd);
                        }
                        sval[0] = 'F';
                        if (!strncmp(str, sval, 2)) {
                            FPUTS("#", tmpfd);
                        }
                        break;

                    default : 
                        break;
                }
            }
            FPUTS(str, tmpfd);
        }
        FCLOSE(rcfd);
    }

    switch (mtype) {
        case M_CON : 
            FPRINTF(tmpfd, "\nC%1d %s %s\n", cfno, val, comment);
            break;

        case M_FUN : 
            FPRINTF(tmpfd, "\nF%1d %s %s\n", cfno, val, comment);
            break;

        default : 
            break;
    }
    UNLINK(rcname);
    rcfd = fopen(rcname, "w");
    REWIND(tmpfd);
    while (fgets(str, MAXLINE, tmpfd)) FPUTS(str, rcfd);
    FCLOSE(rcfd);
    FCLOSE(tmpfd);
    UNLINK(tmp_filename);
}


void
write_resources()
{
    char intval[5];

    SPRINTF(intval, "%d", v->accuracy);
    put_resource(R_ACCURACY, intval);
    put_resource(R_BASE,     Rbstr[(int) v->base]);
    put_resource(R_DISPLAY,  Rdstr[(int) v->dtype]);
    put_resource(R_MODE,     Rmstr[(int) v->modetype]);
    put_resource(R_TRIG,     Rtstr[(int) v->ttype]);
    put_resource(R_REGS,     set_bool(v->rstate == TRUE));
    put_resource(R_RHAND,    set_bool(v->righthand == TRUE));
    put_resource(R_BEEP,     set_bool(v->beep == TRUE));
    save_resources();
}
