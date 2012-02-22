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

with xia_parser_Model;
use xia_parser_Model;

package xia_parser_tokens is


subtype YYSTYPE is xia_parser_Model.Parseable_Ptr;

    YYLVal, YYVal : YYSType;
    type Token is
        (End_Of_Input, Error, Ncname_Token, Decimal_Literal_Token,
         Integer_Token, Dq_Literal_Token, Sq_Literal_Token,
         Le_Token, Lt_Token, Ge_Token,
         Gt_Token, Ne_Token, Eq_Token,
         V_Bar_Token, Dollar_Token, Comma_Token,
         At_Sign_Token, Double_Colon_Token, Colon_Token,
         Double_Slash_Token, Slash_Token, Double_Dot_Token,
         Dot_Token, Minus_Token, Plus_Token,
         Star_Token, R_Bracket_Token, L_Bracket_Token,
         R_Paren_Token, L_Paren_Token, Or_Token,
         Mod_Token, Div_Token, And_Token,
         Node_Token, Processing_Instruction_Token, Text_Token,
         Comment_Token, Self_Token, Preceding_Sibling_Token,
         Preceding_Token, Parent_Token, Namespace_Token,
         Following_Sibling_Token, Following_Token, Descendant_Or_Self_Token,
         Descendant_Token, Child_Token, Attribute_Token,
         Ancestor_Or_Self_Token, Ancestor_Token );

    Syntax_Error : exception;

end Xia_Parser_Tokens;
