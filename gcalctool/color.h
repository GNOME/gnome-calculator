
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

#define CALC_COLOR      "calcolor"
#define CALC_COLORSIZE  20

#define C_WHITE       0
#define C_BLACK       1
#define C_GREY        2
#define C_DECDIG      3
#define C_HEXDIG      4
#define C_ARITHOP     5
#define C_ADJUST      6
#define C_PORTION     7
#define C_FUNC        8
#define C_MAINMODE    9
#define C_PLOGICAL    10
#define C_BLOGICAL    11
#define C_FIN         12
#define C_TRIGMODE    13
#define C_TRIGCOL     14
#define C_SCI         15
#define C_BACK        16
#define C_DISPCOL     17
#define C_MEMORY      18
#define C_TEXT        19

#define calc_colorsetup(r, g, b) \
    (r)[C_WHITE]    = 255; (g)[C_WHITE]    = 255; (b)[C_WHITE]    = 255; \
    (r)[C_BLACK]    =   0; (g)[C_BLACK]    =   0; (b)[C_BLACK]    =   0; \
    (r)[C_GREY]     = 160; (g)[C_GREY]     = 160; (b)[C_GREY]     = 160; \
    (r)[C_DECDIG]   = 170; (g)[C_DECDIG]   = 232; (b)[C_DECDIG]   = 255; \
    (r)[C_HEXDIG]   =   0; (g)[C_HEXDIG]   = 232; (b)[C_HEXDIG]   = 255; \
    (r)[C_ARITHOP]  = 234; (g)[C_ARITHOP]  = 234; (b)[C_ARITHOP]  = 234; \
    (r)[C_ADJUST]   = 255; (g)[C_ADJUST]   = 255; (b)[C_ADJUST]   = 128; \
    (r)[C_PORTION]  = 180; (g)[C_PORTION]  = 255; (b)[C_PORTION]  = 180; \
    (r)[C_FUNC]     =  68; (g)[C_FUNC]     = 187; (b)[C_FUNC]     = 255; \
    (r)[C_MAINMODE] = 255; (g)[C_MAINMODE] = 203; (b)[C_MAINMODE] = 255; \
    (r)[C_PLOGICAL] = 180; (g)[C_PLOGICAL] = 255; (b)[C_PLOGICAL] = 180; \
    (r)[C_BLOGICAL] = 115; (g)[C_BLOGICAL] = 232; (b)[C_BLOGICAL] = 192; \
    (r)[C_FIN]      = 255; (g)[C_FIN]      = 168; (b)[C_FIN]      = 125; \
    (r)[C_TRIGMODE] = 255; (g)[C_TRIGMODE] = 203; (b)[C_TRIGMODE] = 255; \
    (r)[C_TRIGCOL]  = 255; (g)[C_TRIGCOL]  = 147; (b)[C_TRIGCOL]  = 125; \
    (r)[C_SCI]      = 255; (g)[C_SCI]      = 168; (b)[C_SCI]      = 125; \
    (r)[C_BACK]     = 160; (g)[C_BACK]     = 160; (b)[C_BACK]     = 160; \
    (r)[C_DISPCOL]  = 255; (g)[C_DISPCOL]  = 255; (b)[C_DISPCOL]  = 255; \
    (r)[C_MEMORY]   = 255; (g)[C_MEMORY]   = 255; (b)[C_MEMORY]   = 255; \
    (r)[C_TEXT]     =   0; (g)[C_TEXT]     =   0; (b)[C_TEXT]     =   0;
