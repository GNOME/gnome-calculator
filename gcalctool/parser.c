/*  Copyright (C) 2004 Sami Pietila
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

#ifndef PARSER_C
#define PARSER_C

#include <assert.h>
#include <string.h>

#include "calctool.h"
#include "parser.h"

struct parser_state parser_state;

void cp(int s[MP_SIZE], int t[MP_SIZE])
{
  memcpy(t, s, sizeof(int)*MP_SIZE);
}

void ret(int s[MP_SIZE])
{ // copy result value
  memcpy(parser_state.ret, s, sizeof(int)*MP_SIZE);
  parser_state.flags |= ANS;
}

#if 0
void iret(int s[MP_SIZE])
{ // copy intermediate return value
  assert(!parser_state.flags.iret);
  parser_state.flags.iret = 1;
  ret(s);
}


void set_iop(char *op)
{
  assert(op);
  int n = strlen(op);
  assert(n < 8);
  strncpy(parser_state.iop, op, n+1);
}
#endif

#endif
