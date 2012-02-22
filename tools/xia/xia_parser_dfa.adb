------------------------------------------------------------------------
--                                                                    --
--                     McKae Software Utilities                       --
--                                                                    --
--           Copyright (C) 2004 McKae Technologies                    --
--                                                                    --
-- The  McKae   software  utilities   are  free  software;   you  can --
-- redistribute it  and/or modify it  under terms of the  GNU General --
-- Public  License  as published  by  the  Free Software  Foundation; --
-- either version  2, or (at  your option) any later  version.  McKae --
-- Software Utilities are  distributed in the hope that  they will be --
-- useful,  but  WITHOUT  ANY  WARRANTY;  without  even  the  implied --
-- warranty of  MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE. --
-- See the GNU  General Public License for more  details.  You should --
-- have received a copy of the GNU General Public License distributed --
-- with DTraq; see file COPYING.   If not, write to the Free Software --
-- Foundation, 59  Temple Place -  Suite 330, Boston,  MA 02111-1307, --
-- USA.                                                               --
--                                                                    --
-- As a  special exception, if other files  instantiate generics from --
-- this unit,  or you link this  unit with other files  to produce an --
-- executable,  this unit  does  not by  itself  cause the  resulting --
-- executable to be covered by  the GNU General Public License.  This --
-- exception does  not however invalidate  any other reasons  why the --
-- executable file might be covered by the GNU Public License.        --
--                                                                    --
-- The McKae Software Utilities  are maintained by McKae Technologies --
-- (http://www.mckae.com).                                            --
------------------------------------------------------------------------

with xia_parser_dfa; use xia_parser_dfa;
package body xia_parser_dfa is
function YYText return string is
    i : integer;
    str_loc : integer := 1;
    buffer : string(1..1024);
    EMPTY_STRING : constant string := "";
begin
    -- find end of buffer
    i := yytext_ptr;
    while ( yy_ch_buf(i) /= ASCII.NUL ) loop
    buffer(str_loc ) := yy_ch_buf(i);
        i := i + 1;
    str_loc := str_loc + 1;
    end loop;
--    return yy_ch_buf(yytext_ptr.. i - 1);

    if (str_loc < 2) then
        return EMPTY_STRING;
    else
      return buffer(1..str_loc-1);
    end if;

end;

-- returns the length of the matched text
function YYLength return integer is
begin
    return yy_cp - yy_bp;
end YYLength;

-- done after the current pattern has been matched and before the
-- corresponding action - sets up yytext

procedure YY_DO_BEFORE_ACTION is
begin
    yytext_ptr := yy_bp;
    yy_hold_char := yy_ch_buf(yy_cp);
    yy_ch_buf(yy_cp) := ASCII.NUL;
    yy_c_buf_p := yy_cp;
end YY_DO_BEFORE_ACTION;

end xia_parser_dfa;
