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

token_macros

DIGIT            [0-9]
INTEGER          ({DIGIT}{DIGIT}*\.?)
DECIMAL_LITERAL  ({DIGIT}*\.{DIGIT}+)
DQ_LITERAL       (\"[^\"]*\")
SQ_LITERAL       (\'[^\']*\')
LETTER           ([a-zA-Z])
NCNAMECHAR       ({LETTER}|{DIGIT}|[\.\-_])
NCNAME           (({LETTER}|_)({NCNAMECHAR})*)

tokens
-- Reserved Words
ANCESTOR                ancestor
ANCESTOR_OR_SELF        ancestor-or-self
ATTRIBUTE               attribute
CHILD                   child
DESCENDANT              descendant
DESCENDANT_OR_SELF      descendant-or-self
FOLLOWING               following
FOLLOWING_SIBLING       following-sibling
NAMESPACE               namespace
PARENT                  parent
PRECEDING               preceding
PRECEDING_SIBLING       preceding-sibling
SELF                    self

COMMENT                 comment
TEXT                    text
PROCESSING_INSTRUCTION  processing-instruction
NODE                    node


AND   and
DIV   div
MOD   mod
OR    or

-- Delimiters
L_PAREN   "("     
R_PAREN   ")"     
L_BRACKET "["
R_BRACKET "]"

STAR   "*"    
PLUS   "+"     
MINUS   "-"     

DOT   "."     
DOUBLE_DOT   ".."
SLASH   "/"     
DOUBLE_SLASH   "//"
COLON   ":"     
DOUBLE_COLON   "::"

AT_SIGN   "@"

COMMA    ","
DOLLAR   "$"
V_BAR    "|"

EQ   "="     
NE   "!="    
GT   ">"     
GE   ">="   
LT   "<"     
LE   "<="   

-- Literals
SQ_LITERAL  {SQ_LITERAL}
DQ_LITERAL  {DQ_LITERAL}
INTEGER         {INTEGER}
DECIMAL_LITERAL {DECIMAL_LITERAL}
NCNAME          {NCNAME}

--Some code removed here related to dealing with situations like x'a'b that make
--dealing with one character literals a little harder

-- Whitespace and Comments
--Must be "ignore", special connotations for ADAGoop
--ignore   [\ \t\r\n]*        

global_methods
Pathify

--XPath grammar goes here
grammar
%start Location_Path


Location_Path : Relative_Location_Path
                | Absolute_Location_Path
                ;   

Absolute_Location_Path : SLASH 
                    | SLASH Relative_Location_Path
                    | Abbreviated_Absolute_Location_Path
                    ;

Relative_Location_Path : Step
                     | Relative_Location_Path SLASH Step
                     | Abbreviated_Relative_Location_Path
                     ;

Step                 : Step_Base Predicates
                     | Abbreviated_Step
                     ;

Predicates          :
                    | Predicates Predicate
                    ;

Step_Base           : Axis_Specifier Node_Test
                    | Abbreviated_Step_Base
                    ;

Abbreviated_Step_Base : Node_Test
                    | AT_SIGN Node_Test
                    ;

Predicate            : L_BRACKET Predicate_Expr R_BRACKET
                     ;

Axis_Specifier       : Axis_Name DOUBLE_COLON
                     ;

Axis_Name            : ANCESTOR
                     | ANCESTOR_OR_SELF
                     | ATTRIBUTE
                     | CHILD
                     | DESCENDANT
                     | DESCENDANT_OR_SELF
                     | FOLLOWING
                     | FOLLOWING_SIBLING
                     | NAMESPACE
                     | PARENT
                     | PRECEDING
                     | PRECEDING_SIBLING
                     | SELF
                     ;

Node_Test             : Name_Test
                     | Node_Type L_PAREN R_PAREN
                     | PROCESSING_INSTRUCTION L_PAREN LITERAL R_PAREN
                     ;

Node_Type            : COMMENT
                    | TEXT
                    | PROCESSING_INSTRUCTION
                    | NODE
                    ;

Name_Test            : STAR
                    | QName
                    | NCNAME_Or_ID COLON STAR
                    ;

QName               : NCNAME_Or_ID
                    | NCNAME_Or_ID COLON NCNAME_Or_ID
                    ;

NCNAME_Or_ID        : NCNAME
                    | Axis_Name
                    | Node_Type
                    | AND
                    | OR
                    | MOD
                    | DIV
                    ;

Predicate_Expr       : Expr
                     ;

Abbreviated_Absolute_Location_Path : DOUBLE_SLASH Relative_Location_Path
                     ;

Abbreviated_Relative_Location_Path : Relative_Location_Path DOUBLE_SLASH Step
                     ;

Abbreviated_Step     : DOT
                     | DOUBLE_DOT
                     ;

Expr               : Or_Expr
                   ;

Primary_Expr         : Variable_Reference
                    | L_PAREN Expr R_PAREN
                    | LITERAL
                    | Number
                    | Function_Call
                    ;

Variable_Reference   : DOLLAR QName
                    ;

Function_Call        : Function_Name L_PAREN Arguments R_PAREN
                    ;

Arguments           :
                    | Argument
                    | Arguments COMMA Argument
                    ;

Argument            : Expr
                    ;

Function_Name       : QName
                    ;

Union_Expr          : Path_Expr
                    | Union_Expr V_BAR Path_Expr
                    ;

Path_Expr           : Location_Path
                    | Filter_Expr
                    | Filter_Expr SLASH Relative_Location_Path
                    | Filter_Expr DOUBLE_SLASH Relative_Location_Path
                    ;

Filter_Expr         : Primary_Expr
                    | Filter_Expr Predicate
                    ;

Or_Expr             : And_Expr
                    | Or_Expr OR And_Expr
                    ;

And_Expr            : Equality_Expr
                    | And_Expr AND Equality_Expr
                    ;

Equality_Expr       : Relational_Expr
                    | Equality_Expr EQ Relational_Expr
                    | Equality_Expr NE Relational_Expr
                    ;

Relational_Expr     : Additive_Expr
                    | Relational_Expr LT Additive_Expr
                    | Relational_Expr GT Additive_Expr
                    | Relational_Expr LE Additive_Expr
                    | Relational_Expr GE Additive_Expr
                    ;

Additive_Expr       : Multiplicative_Expr
                    | Additive_Expr PLUS Multiplicative_Expr
                    | Additive_Expr MINUS Multiplicative_Expr
                    ;

Multiplicative_Expr : Unary_Expr
                    | Multiplicative_Expr STAR Unary_Expr
                    | Multiplicative_Expr DIV Unary_Expr
                    | Multiplicative_Expr MOD Unary_Expr
                    ;

Unary_Expr          : Union_Expr
                    | MINUS Unary_Expr
                    ;

Number              : INTEGER
                    | DECIMAL_LITERAL
                    ;
                
Literal             : DQ_LITERAL
                    | SQ_LITERAL
                    ;
