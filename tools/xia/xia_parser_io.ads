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

package xia_parser_io is

   NULL_IN_INPUT : exception;
   AFLEX_INTERNAL_ERROR : exception;
   UNEXPECTED_LAST_MATCH : exception;
   PUSHBACK_OVERFLOW : exception;
   AFLEX_SCANNER_JAMMED : exception;
   type eob_action_type is ( EOB_ACT_RESTART_SCAN,
                             EOB_ACT_END_OF_FILE,
                             EOB_ACT_LAST_MATCH );
   YY_END_OF_BUFFER_CHAR :  constant character:=  ASCII.NUL;
   yy_n_chars : integer;       -- number of characters read into yy_ch_buf

   -- true when we've seen an EOF for the current input file
   yy_eof_has_been_seen : boolean;

   procedure YY_INPUT(buf: out unbounded_character_array; result: out integer; max_size: in integer);
   function yy_get_next_buffer return eob_action_type;
   procedure yyunput( c : character; yy_bp: in out integer );
   procedure unput(c : character);
   function input return character;
--   procedure output(c : character);
   function yywrap return boolean;

   procedure Open_Input(Set_String : in String);
   procedure Close_Input;
   --   procedure Create_Output(fname : in String := "");
--   procedure Close_Output;

end xia_parser_io;
