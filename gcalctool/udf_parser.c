
/*  $Header$
 *
 *  Copyright (C) 2004 Sami Pietila
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

#include "udf_parser.h"
#include "limits.h"
#include "extern.h"
#include "calctool.h"
//#include "udf_parser.tab.h"

extern struct parser_state parser_state;

int udf_parse(char *expression,
	     int result[MP_SIZE])
{
  int ret = 0;

  if (!(expression && result))
    return -EINVAL;

  memset(&parser_state, 0, sizeof(struct parser_state));

  if (strlen(expression)) {
    parser_state.i = 0;
    parser_state.buff = expression;
    ret = udfparse();
  }
  
  if (ret || parser_state.error) {
    return (parser_state.error) ? parser_state.error : ret;
  } else {
    memcpy(result, parser_state.ret, sizeof(int)*MP_SIZE);
    return 0;
  }

}
