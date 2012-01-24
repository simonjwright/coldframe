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

with xia_parser_IO;
package body xia_parser is
   Current_Line : Natural := 1;
   Current_Column : Natural := 1;
   procedure Reset(Set_String : in String) is
   begin
      Current_Line := 1;
      Current_Column := 1;
      xia_parser_IO.Open_Input (Set_String);
--      xia_parser_IO.Create_Output;
   end Reset;

   procedure Close_Files is
   begin
      xia_parser_IO.Close_Input;
--      xia_parser_IO.Close_Output;
   end Close_Files;

   function YYLex return Token is
subtype short is integer range -32768..32767;
    yy_act : integer;
    yy_c : short;

-- returned upon end-of-file
YY_END_TOK : constant integer := 0;
YY_END_OF_BUFFER : constant := 53;
subtype yy_state_type is integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
yy_accept : constant array(0..183) of short :=
    (   0,
        0,    0,   53,   52,   50,   51,   52,   52,   37,   52,
       22,   23,   26,   27,   36,   28,   29,   31,   47,   33,
       43,   39,   41,   35,   49,   24,   25,   49,   49,   49,
       49,   49,   49,   49,   49,   49,   49,   38,   40,    0,
       46,    0,   45,   30,   48,   32,   47,   47,   34,   44,
       42,   49,   49,   49,   49,   49,   49,   49,   49,   49,
       49,   49,   21,   49,   49,   49,   49,   49,   18,   49,
       49,   49,   49,   19,   49,   20,   49,   49,   49,   49,
       49,   49,   49,   49,   49,   49,   49,   49,   49,   49,
       17,   49,   49,   49,   13,   15,   49,   49,    4,   49,

       49,   49,   49,   49,   49,   49,   49,   49,   49,   49,
       49,   49,   10,   49,   49,   49,   49,   14,   49,   49,
       49,   49,   49,    1,   49,   49,   49,   49,   49,   49,
       49,    3,   49,    7,    9,   11,   49,   49,    5,   49,
       49,   49,   49,   49,   49,   49,   49,   49,   49,   49,
       49,   49,   49,   49,   49,   49,   49,   49,   49,   49,
       49,   49,   49,   49,   49,   49,   49,    2,   49,   49,
       49,   49,   49,    8,   12,   49,    6,   49,   49,   49,
       49,   16,    0
    ) ;

yy_ec : constant array(CHARACTER'FIRST..ASCII.DEL) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    4,    5,    1,    6,    1,    1,    7,    8,
        9,   10,   11,   12,   13,   14,   15,   16,   16,   16,
       16,   16,   16,   16,   16,   16,   16,   17,    1,   18,
       19,   20,    1,   21,   22,   22,   22,   22,   22,   22,
       22,   22,   22,   22,   22,   22,   22,   22,   22,   22,
       22,   22,   22,   22,   22,   22,   22,   22,   22,   22,
       23,    1,   24,    1,   25,    1,   26,   27,   28,   29,

       30,   31,   32,   33,   34,   22,   22,   35,   36,   37,
       38,   39,   22,   40,   41,   42,   43,   44,   45,   46,
       22,   22,    1,   47,    1,    1,    1
    ) ;

yy_meta : constant array(0..47) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    2,    2,    1,    2,    1,    1,    1,    1,
        1,    2,    1,    1,    2,    2,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    1
    ) ;

yy_base : constant array(0..186) of short :=
    (   0,
        0,    0,  200,  201,  201,  201,  180,  193,  201,  190,
      201,  201,  201,  201,  201,  201,   34,  181,   35,  178,
      175,  201,  174,  201,    0,  201,  201,   15,   20,   25,
      154,  153,   28,  150,   30,  159,  158,  201,  201,  182,
      201,  179,  201,  201,  169,  201,  168,   46,  201,  201,
      201,    0,   35,  141,  148,  145,  139,  135,  143,  148,
      140,  146,    0,  134,   31,  138,  126,  141,    0,  130,
      134,  132,  139,    0,  131,    0,  135,  134,  133,  134,
      133,  129,  117,  117,  123,  127,  125,  124,  115,  111,
        0,  114,  120,  119,    0,    0,  106,  120,    0,  109,

      108,   99,  104,  100,  112,   99,  101,   95,   95,  107,
      101,  108,    0,   99,   91,   91,   88,    0,  103,   91,
       99,   89,   91,  111,   93,   85,   89,   90,   87,   81,
       79,    0,   74,  102,    0,  101,   81,   72,   98,   69,
       68,   95,   94,   68,   71,   70,   69,   61,   61,   73,
       72,   61,   67,   83,   60,   59,   52,   57,   50,   56,
       55,   46,   56,   56,   48,   47,   43,    0,   47,   49,
       48,   36,   47,    0,    0,   49,    0,   34,   41,   30,
       30,    0,  201,   70,   72,   63
    ) ;

yy_def : constant array(0..186) of short :=
    (   0,
      183,    1,  183,  183,  183,  183,  183,  184,  183,  185,
      183,  183,  183,  183,  183,  183,  183,  183,  183,  183,
      183,  183,  183,  183,  186,  183,  183,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  183,  183,  184,
      183,  185,  183,  183,  183,  183,  183,  183,  183,  183,
      183,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,

      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,    0,  183,  183,  183
    ) ;

yy_nxt : constant array(0..248) of short :=
    (   0,
        4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
       14,   15,   16,   17,   18,   19,   20,   21,   22,   23,
       24,   25,   26,   27,   25,   28,   25,   29,   30,   25,
       31,   25,   25,   25,   25,   32,   33,   34,   35,   25,
       36,   37,   25,   25,   25,   25,   38,   44,   47,   45,
       48,   53,   55,   61,   57,   64,   54,   56,   58,   47,
       80,   48,   68,   69,   52,   62,  182,  181,   81,   65,
       40,   40,   42,   42,  180,  179,  178,  177,  176,  175,
      174,  173,  172,  171,  170,  169,  168,  167,  166,  165,
      164,  163,  162,  161,  160,  159,  158,  157,  156,  155,

      154,  153,  152,  151,  150,  149,  148,  147,  146,  145,
      144,  143,  142,  141,  140,  139,  138,  137,  136,  135,
      134,  133,  132,  131,  130,  129,  128,  127,  126,  125,
      124,  123,  122,  121,  120,  119,  118,  117,  116,  115,
      114,  113,  112,  111,  110,  109,  108,  107,  106,  105,
      104,  103,  102,  101,  100,   99,   98,   97,   96,   95,
       94,   93,   92,   91,   90,   89,   88,   87,   86,   85,
       84,   83,   82,   79,   78,   77,   76,   75,   74,   73,
       72,   71,   70,   45,   45,   43,   41,   67,   66,   63,
       60,   59,   51,   50,   49,   46,   43,   41,   39,  183,

        3,  183,  183,  183,  183,  183,  183,  183,  183,  183,
      183,  183,  183,  183,  183,  183,  183,  183,  183,  183,
      183,  183,  183,  183,  183,  183,  183,  183,  183,  183,
      183,  183,  183,  183,  183,  183,  183,  183,  183,  183,
      183,  183,  183,  183,  183,  183,  183,  183
    ) ;

yy_chk : constant array(0..248) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,   17,   19,   17,
       19,   28,   29,   33,   30,   35,   28,   29,   30,   48,
       65,   48,   53,   53,  186,   33,  181,  180,   65,   35,
      184,  184,  185,  185,  179,  178,  176,  173,  172,  171,
      170,  169,  167,  166,  165,  164,  163,  162,  161,  160,
      159,  158,  157,  156,  155,  154,  153,  152,  151,  150,

      149,  148,  147,  146,  145,  144,  143,  142,  141,  140,
      139,  138,  137,  136,  134,  133,  131,  130,  129,  128,
      127,  126,  125,  124,  123,  122,  121,  120,  119,  117,
      116,  115,  114,  112,  111,  110,  109,  108,  107,  106,
      105,  104,  103,  102,  101,  100,   98,   97,   94,   93,
       92,   90,   89,   88,   87,   86,   85,   84,   83,   82,
       81,   80,   79,   78,   77,   75,   73,   72,   71,   70,
       68,   67,   66,   64,   62,   61,   60,   59,   58,   57,
       56,   55,   54,   47,   45,   42,   40,   37,   36,   34,
       32,   31,   23,   21,   20,   18,   10,    8,    7,    3,

      183,  183,  183,  183,  183,  183,  183,  183,  183,  183,
      183,  183,  183,  183,  183,  183,  183,  183,  183,  183,
      183,  183,  183,  183,  183,  183,  183,  183,  183,  183,
      183,  183,  183,  183,  183,  183,  183,  183,  183,  183,
      183,  183,  183,  183,  183,  183,  183,  183
    ) ;


-- copy whatever the last rule matched to the standard output

--  procedure ECHO is
--  begin
--     if (text_io.is_open(user_output_file)) then
--       text_io.put( user_output_file, yytext );
--     else
--       text_io.put( yytext );
--     end if;
--  end ECHO;

-- enter a start condition.
-- Using procedure requires a () after the ENTER, but makes everything
-- much neater.

procedure ENTER( state : integer ) is
begin
     yy_start := 1 + 2 * state;
end ENTER;

-- action number for EOF rule of a given start state
function YY_STATE_EOF(state : integer) return integer is
begin
     return YY_END_OF_BUFFER + state + 1;
end YY_STATE_EOF;

-- return all but the first 'n' matched characters back to the input stream
procedure yyless(n : integer) is
begin
        yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
        yy_cp := yy_bp + n;
        yy_c_buf_p := yy_cp;
        YY_DO_BEFORE_ACTION; -- set up yytext again
end yyless;

-- redefine this if you have something you want each time.
procedure YY_USER_ACTION is
begin
        null;
end;

-- yy_get_previous_state - get the state just before the EOB char was reached

function yy_get_previous_state return yy_state_type is
    yy_current_state : yy_state_type;
    yy_c : short;
begin
    yy_current_state := yy_start;

    for yy_cp in yytext_ptr..yy_c_buf_p - 1 loop
        yy_c := yy_ec(yy_ch_buf(yy_cp));
        if ( yy_accept(yy_current_state) /= 0 ) then
            yy_last_accepting_state := yy_current_state;
            yy_last_accepting_cpos := yy_cp;
        end if;
        while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
            yy_current_state := yy_def(yy_current_state);
            if ( yy_current_state >= 184 ) then
                yy_c := yy_meta(yy_c);
            end if;
        end loop;
        yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
    end loop;

    return yy_current_state;
end yy_get_previous_state;

procedure yyrestart( input_file : file_type ) is
begin
   open_input(text_io.name(input_file));
end yyrestart;

begin -- of YYLex
<<new_file>>
        -- this is where we enter upon encountering an end-of-file and
        -- yywrap() indicating that we should continue processing

    if ( yy_init ) then
        if ( yy_start = 0 ) then
            yy_start := 1;      -- first start state
        end if;

        -- we put in the '\n' and start reading from [1] so that an
        -- initial match-at-newline will be true.

        yy_ch_buf(0) := ASCII.LF;
        yy_n_chars := 1;

        -- we always need two end-of-buffer characters.  The first causes
        -- a transition to the end-of-buffer state.  The second causes
        -- a jam in that state.

        yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
        yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

        yy_eof_has_been_seen := false;

        yytext_ptr := 1;
        yy_c_buf_p := yytext_ptr;
        yy_hold_char := yy_ch_buf(yy_c_buf_p);
        yy_init := false;
    end if; -- yy_init

    loop                -- loops until end-of-file is reached
        yy_cp := yy_c_buf_p;

        -- support of yytext
        yy_ch_buf(yy_cp) := yy_hold_char;

        -- yy_bp points to the position in yy_ch_buf of the start of the
        -- current run.
        yy_bp := yy_cp;
        yy_current_state := yy_start;
        loop
                yy_c := yy_ec(yy_ch_buf(yy_cp));
                if ( yy_accept(yy_current_state) /= 0 ) then
                    yy_last_accepting_state := yy_current_state;
                    yy_last_accepting_cpos := yy_cp;
                end if;
                while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
                    yy_current_state := yy_def(yy_current_state);
                    if ( yy_current_state >= 184 ) then
                        yy_c := yy_meta(yy_c);
                    end if;
                end loop;
                yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
            yy_cp := yy_cp + 1;
if ( yy_current_state = 183 ) then
    exit;
end if;
        end loop;
        yy_cp := yy_last_accepting_cpos;
        yy_current_state := yy_last_accepting_state;

<<next_action>>
            yy_act := yy_accept(yy_current_state);
            YY_DO_BEFORE_ACTION;
            YY_USER_ACTION;

        if aflex_debug then  -- output acceptance info. for (-d) debug mode
            text_io.put("--accepting rule #" );
            text_io.put(INTEGER'IMAGE(yy_act) );
            text_io.put_line("(""" & yytext & """)");
        end if;

<<do_action>>   -- this label is used only to access EOF actions
            case yy_act is
                when 0 => -- must backtrack
                -- undo the effects of YY_DO_BEFORE_ACTION
                yy_ch_buf(yy_cp) := yy_hold_char;
                yy_cp := yy_last_accepting_cpos;
                yy_current_state := yy_last_accepting_state;
                goto next_action;


when 1 =>
--# line 15 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (ANCESTOR_token);

when 2 =>
--# line 16 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (ANCESTOR_OR_SELF_token);

when 3 =>
--# line 17 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (ATTRIBUTE_token);

when 4 =>
--# line 18 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (CHILD_token);

when 5 =>
--# line 19 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (DESCENDANT_token);

when 6 =>
--# line 20 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (DESCENDANT_OR_SELF_token);

when 7 =>
--# line 21 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (FOLLOWING_token);

when 8 =>
--# line 22 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (FOLLOWING_SIBLING_token);

when 9 =>
--# line 23 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (NAMESPACE_token);

when 10 =>
--# line 24 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (PARENT_token);

when 11 =>
--# line 25 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (PRECEDING_token);

when 12 =>
--# line 26 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (PRECEDING_SIBLING_token);

when 13 =>
--# line 27 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (SELF_token);

when 14 =>
--# line 28 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (COMMENT_token);

when 15 =>
--# line 29 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (TEXT_token);

when 16 =>
--# line 30 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (PROCESSING_INSTRUCTION_token);

when 17 =>
--# line 31 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (NODE_token);

when 18 =>
--# line 32 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (AND_token);

when 19 =>
--# line 33 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (DIV_token);

when 20 =>
--# line 34 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (MOD_token);

when 21 =>
--# line 35 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (OR_token);

when 22 =>
--# line 36 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (L_PAREN_token);

when 23 =>
--# line 37 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (R_PAREN_token);

when 24 =>
--# line 38 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (L_BRACKET_token);

when 25 =>
--# line 39 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (R_BRACKET_token);

when 26 =>
--# line 40 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (STAR_token);

when 27 =>
--# line 41 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (PLUS_token);

when 28 =>
--# line 42 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (MINUS_token);

when 29 =>
--# line 43 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (DOT_token);

when 30 =>
--# line 44 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (DOUBLE_DOT_token);

when 31 =>
--# line 45 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (SLASH_token);

when 32 =>
--# line 46 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (DOUBLE_SLASH_token);

when 33 =>
--# line 47 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (COLON_token);

when 34 =>
--# line 48 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (DOUBLE_COLON_token);

when 35 =>
--# line 49 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (AT_SIGN_token);

when 36 =>
--# line 50 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (COMMA_token);

when 37 =>
--# line 51 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (DOLLAR_token);

when 38 =>
--# line 52 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (V_BAR_token);

when 39 =>
--# line 53 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (EQ_token);

when 40 =>
--# line 54 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (NE_token);

when 41 =>
--# line 55 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (GT_token);

when 42 =>
--# line 56 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (GE_token);

when 43 =>
--# line 57 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (LT_token);

when 44 =>
--# line 58 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (LE_token);

when 45 =>
--# line 59 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (SQ_LITERAL_token);

when 46 =>
--# line 60 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (DQ_LITERAL_token);

when 47 =>
--# line 61 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (INTEGER_token);

when 48 =>
--# line 62 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (DECIMAL_LITERAL_token);

when 49 =>
--# line 63 "xia_parser.l"
Current_Column := Current_Column + YYText'Length; return (NCNAME_token);

when 50 =>
--# line 64 "xia_parser.l"
Current_Column := Current_Column + 1;

when 51 =>
--# line 65 "xia_parser.l"
Current_Line := Current_Line + 1; Current_Column := 1;

when 52 =>
--# line 66 "xia_parser.l"
--ECHO;
   null;
when YY_END_OF_BUFFER + INITIAL + 1 =>
    return End_Of_Input;
                when YY_END_OF_BUFFER =>
                    -- undo the effects of YY_DO_BEFORE_ACTION
                    yy_ch_buf(yy_cp) := yy_hold_char;

                    yytext_ptr := yy_bp;

                    case yy_get_next_buffer is
                        when EOB_ACT_END_OF_FILE =>
                            begin
                            if ( yywrap ) then
                                -- note: because we've taken care in
                                -- yy_get_next_buffer() to have set up yytext,
                                -- we can now set up yy_c_buf_p so that if some
                                -- total hoser (like aflex itself) wants
                                -- to call the scanner after we return the
                                -- End_Of_Input, it'll still work - another
                                -- End_Of_Input will get returned.

                                yy_c_buf_p := yytext_ptr;

                                yy_act := YY_STATE_EOF((yy_start - 1) / 2);

                                goto do_action;
                            else
                                --  start processing a new file
                                yy_init := true;
                                goto new_file;
                            end if;
                            end;
                        when EOB_ACT_RESTART_SCAN =>
                            yy_c_buf_p := yytext_ptr;
                            yy_hold_char := yy_ch_buf(yy_c_buf_p);
                        when EOB_ACT_LAST_MATCH =>
                            yy_c_buf_p := yy_n_chars;
                            yy_current_state := yy_get_previous_state;

                            yy_cp := yy_c_buf_p;
                            yy_bp := yytext_ptr;
                            goto next_action;
                        when others => null;
                        end case; -- case yy_get_next_buffer()
                when others =>
                    text_io.put( "action # " );
                    text_io.put( INTEGER'IMAGE(yy_act) );
                    text_io.new_line;
                    raise AFLEX_INTERNAL_ERROR;
            end case; -- case (yy_act)
        end loop; -- end of loop waiting for end of file
end YYLex;
--# line 66 "xia_parser.l"

   function Get_Token return Token is
   begin
      return YYLex;
   end Get_Token;

   function Get_Token_String return String is
   begin
      return YYText;
   end Get_Token_String;

   function Get_Current_Line return Natural is
   begin
      return Current_Line;
   end Get_Current_Line;

   function Get_Current_Column return Natural is
   begin
      return Current_Column;
   end Get_Current_Column;

end xia_parser;

