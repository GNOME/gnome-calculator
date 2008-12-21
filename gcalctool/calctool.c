
/*  $Header$
 *
 *  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
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
#include <assert.h>
#include <sys/types.h>

#include "calctool.h"
#include "unittest.h"
#include "get.h"
#include "display.h"
#include "functions.h"
#include "ui.h"
#include "mp.h"
#include "register.h"

time_t time();

int basevals[4] = { 2, 8, 10, 16 };

/* Calctool variables and options. */
static CalculatorVariables calc_state;
CalculatorVariables *v;

/* Change type to radian */
void
to_rad(int s1[MP_SIZE], int t1[MP_SIZE])
{
    int MP1[MP_SIZE], MP2[MP_SIZE];

    if (v->ttype == DEG) {
        mp_get_pi(MP1);
        mpmul(s1, MP1, MP2);
        mp_set_from_integer(180, MP1);
        mpdiv(MP2, MP1, t1);
    } else if (v->ttype == GRAD) {
        mp_get_pi(MP1);
        mpmul(s1, MP1, MP2);
        mp_set_from_integer(200, MP1);
        mpdiv(MP2, MP1, t1);
    } else {
        mp_set_from_mp(s1, t1);
    }
}

void
do_trig_typeconv(TrigType ttype, int s1[MP_SIZE], int t1[MP_SIZE])
{
    int MP1[MP_SIZE], MP2[MP_SIZE];
  
    switch (ttype) {

        case DEG:
            mp_set_from_integer(180, MP1);
            mpmul(s1, MP1, MP2);
            mp_get_pi(MP1);
            mpdiv(MP2, MP1, t1);
            break;

        case RAD:
            mp_set_from_mp(s1, t1);
            break;

        case GRAD:
            mp_set_from_integer(200, MP1);
            mpmul(s1, MP1, MP2);
            mp_get_pi(MP1);
            mpdiv(MP2, MP1, t1);
            break;

        default:
            assert(0);
            break;
    }
}

/* Calctools' customised math library error-handling routine. */
void
doerr(char *errmes)
{
    v->math_error = -MPMATH_ERR;
}

/* Default math library exception handling routine. */

/*ARGSUSED*/
int
matherr(struct exception *exc)
{
    // FIXME: Useless string
    doerr(_("Error"));

    return(1);
}


static void
usage(const char *progname)
{
    /* Translators: This message is displayed on the command line when
       help is requested. %1$s and $3$s are replaced with the name
       of the program and %2$s with the version string */
    fprintf(stderr, _("%1$s version %2$s\n"
                      "\n"
                      "Usage: %3$s: [-u] [-?] [-v] [-h]\n"),
            progname, VERSION, progname);
    exit(1);
}

void
get_options(int argc, char *argv[])
{
    int i;
    char *arg;
   
    for (i = 1; i < argc; i++) {
       arg = argv[i];
       if (arg[0] == '-' && arg[2] == '\0')
       {
          switch (arg[1]) {
          case 'u':
             unittest();
             break;
             
          case '?' :
          case 'v' : 
          case 'h' :                 
             usage(v->progname);
             break;
          }
       }
    }
}


static void
init_state(void)
{
    int acc, size, i;

    acc              = MAX_DIGITS + 12;     /* MP internal accuracy. */
    size             = MP_SIZE;
    mpset(acc, size, size);

    v->error         = FALSE;  /* No calculator error initially. */
    v->radix         = get_radix();    /* Locale specific radix string. */
    v->tsep          = get_tsep();     /* Locale specific thousands separator. */
    v->tsep_count    = get_tsep_count();
   
    if (get_int_resource(R_ACCURACY, &i))
        v->accuracy = i;
    else
        v->accuracy = DEFAULT_ACCURACY;
    if (v->accuracy < 0 || v->accuracy > MAXACC) {
       /* Translators: A log message displayed when an invalid accuracy
        is read from the configuration */
       fprintf(stderr, _("%s: accuracy should be in the range 0-%d\n"), 
               v->progname, MAXACC);
       v->accuracy = DEFAULT_ACCURACY;
    }

    if (get_enumerated_resource(R_BASE, Rbstr, &i))
       v->base = (BaseType) i;
    else
       v->base = DEC;

    if (get_enumerated_resource(R_TRIG, Rtstr, &i))
       v->ttype = (TrigType) i;
    else
       v->ttype = DEG;  
}


int
main(int argc, char **argv)
{
    memset(&calc_state, 0, sizeof(calc_state));
    v = &calc_state;

    bindtextdomain(GETTEXT_PACKAGE, PACKAGE_LOCALE_DIR);
    bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
    textdomain(GETTEXT_PACKAGE);

    v->progname = argv[0];
    
    /* Seed random number generator. */   
    srand48((long) time((time_t *) 0));
   
    resources_init();
    
    init_state();
    register_init();
    display_init(&v->display);
    ui_init(&argc, &argv);
   
    get_options(argc, argv);

    ui_load();
    ui_start();
    
    return(0);
}
