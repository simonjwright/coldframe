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
-- Automatically generated AdaGOOP output
-- (by Martin C. Carlisle)
-- If you modify, be careful about losing your work!
------------------------------------------------------------------------

with xia_parser;
with Mckae.XML.XPath.Expressions;
use  Mckae.XML.XPath.Expressions;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package xia_parser_Model is
   subtype String_Ptr is McKae.XML.XPath.Expressions.String_Ptr;

   type Parseable is abstract tagged null record;
   procedure Pathify(This : in out Parseable) is abstract;
   procedure Evaluate (This         : in     Parseable;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is abstract;
   type Parseable_Ptr is access all Parseable'Class;

   type Parseable_Token is new Parseable with record
      Line         : Natural;
      Column       : Natural;
      Token_String : String_Ptr;
      Token_Type   : xia_parser.Token;
   end record;
   procedure Pathify(This : in out Parseable_Token);
   procedure Evaluate (This : in Parseable_Token;
                       Context_Node : in     Node_Items;
                       Value : out Expression_Values);
   type Parseable_Token_Ptr is access all Parseable_Token'Class;

   type Literal_nonterminal;
   type Literal_nonterminal_ptr is access all Literal_nonterminal'Class;
   type Number_nonterminal;
   type Number_nonterminal_ptr is access all Number_nonterminal'Class;
   type Unary_Expr_nonterminal;
   type Unary_Expr_nonterminal_ptr is access all Unary_Expr_nonterminal'Class;
   type Multiplicative_Expr_nonterminal;
   type Multiplicative_Expr_nonterminal_ptr is access all Multiplicative_Expr_nonterminal'Class;
   type Additive_Expr_nonterminal;
   type Additive_Expr_nonterminal_ptr is access all Additive_Expr_nonterminal'Class;
   type Relational_Expr_nonterminal;
   type Relational_Expr_nonterminal_ptr is access all Relational_Expr_nonterminal'Class;
   type Equality_Expr_nonterminal;
   type Equality_Expr_nonterminal_ptr is access all Equality_Expr_nonterminal'Class;
   type And_Expr_nonterminal;
   type And_Expr_nonterminal_ptr is access all And_Expr_nonterminal'Class;
   type Or_Expr_nonterminal;
   type Or_Expr_nonterminal_ptr is access all Or_Expr_nonterminal'Class;
   type Filter_Expr_nonterminal;
   type Filter_Expr_nonterminal_ptr is access all Filter_Expr_nonterminal'Class;
   type Path_Expr_nonterminal;
   type Path_Expr_nonterminal_ptr is access all Path_Expr_nonterminal'Class;
   type Union_Expr_nonterminal;
   type Union_Expr_nonterminal_ptr is access all Union_Expr_nonterminal'Class;
   type Function_Name_nonterminal;
   type Function_Name_nonterminal_ptr is access all Function_Name_nonterminal'Class;
   type Argument_nonterminal;
   type Argument_nonterminal_ptr is access all Argument_nonterminal'Class;
   type Arguments_nonterminal;
   type Arguments_nonterminal_ptr is access all Arguments_nonterminal'Class;
   type Function_Call_nonterminal;
   type Function_Call_nonterminal_ptr is access all Function_Call_nonterminal'Class;
   type Variable_Reference_nonterminal;
   type Variable_Reference_nonterminal_ptr is access all Variable_Reference_nonterminal'Class;
   type Primary_Expr_nonterminal;
   type Primary_Expr_nonterminal_ptr is access all Primary_Expr_nonterminal'Class;
   type Expr_nonterminal;
   type Expr_nonterminal_ptr is access all Expr_nonterminal'Class;
   type Abbreviated_Step_nonterminal;
   type Abbreviated_Step_nonterminal_ptr is access all Abbreviated_Step_nonterminal'Class;
   type Abbreviated_Relative_Location_Path_nonterminal;
   type Abbreviated_Relative_Location_Path_nonterminal_ptr is access all Abbreviated_Relative_Location_Path_nonterminal'Class;
   type Abbreviated_Absolute_Location_Path_nonterminal;
   type Abbreviated_Absolute_Location_Path_nonterminal_ptr is access all Abbreviated_Absolute_Location_Path_nonterminal'Class;
   type Predicate_Expr_nonterminal;
   type Predicate_Expr_nonterminal_ptr is access all Predicate_Expr_nonterminal'Class;
   type NCNAME_Or_ID_nonterminal;
   type NCNAME_Or_ID_nonterminal_ptr is access all NCNAME_Or_ID_nonterminal'Class;
   type QName_nonterminal;
   type QName_nonterminal_ptr is access all QName_nonterminal'Class;
   type Name_Test_nonterminal;
   type Name_Test_nonterminal_ptr is access all Name_Test_nonterminal'Class;
   type Node_Type_nonterminal;
   type Node_Type_nonterminal_ptr is access all Node_Type_nonterminal'Class;
   type Node_Test_nonterminal;
   type Node_Test_nonterminal_ptr is access all Node_Test_nonterminal'Class;
   type Axis_Name_nonterminal;
   type Axis_Name_nonterminal_ptr is access all Axis_Name_nonterminal'Class;
   type Axis_Specifier_nonterminal;
   type Axis_Specifier_nonterminal_ptr is access all Axis_Specifier_nonterminal'Class;
   type Predicate_nonterminal;
   type Predicate_nonterminal_ptr is access all Predicate_nonterminal'Class;
   type Abbreviated_Step_Base_nonterminal;
   type Abbreviated_Step_Base_nonterminal_ptr is access all Abbreviated_Step_Base_nonterminal'Class;
   type Step_Base_nonterminal;
   type Step_Base_nonterminal_ptr is access all Step_Base_nonterminal'Class;
   type Predicates_nonterminal;
   type Predicates_nonterminal_ptr is access all Predicates_nonterminal'Class;
   type Step_nonterminal;
   type Step_nonterminal_ptr is access all Step_nonterminal'Class;
   type Relative_Location_Path_nonterminal;
   type Relative_Location_Path_nonterminal_ptr is access all Relative_Location_Path_nonterminal'Class;
   type Absolute_Location_Path_nonterminal;
   type Absolute_Location_Path_nonterminal_ptr is access all Absolute_Location_Path_nonterminal'Class;
   type Location_Path_nonterminal;
   type Location_Path_nonterminal_ptr is access all Location_Path_nonterminal'Class;

   type Location_Path_nonterminal is abstract new Parseable with null record;
   type Location_Path_nonterminal1 is new Location_Path_nonterminal with record
      Relative_Location_Path_part : Relative_Location_Path_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Location_Path_nonterminal1);
   procedure Evaluate(This : in Location_Path_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Location_Path_nonterminal2 is new Location_Path_nonterminal with record
      Absolute_Location_Path_part : Absolute_Location_Path_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Location_Path_nonterminal2);
   procedure Evaluate(This : in Location_Path_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Absolute_Location_Path_nonterminal is abstract new Parseable with null record;
   type Absolute_Location_Path_nonterminal1 is new Absolute_Location_Path_nonterminal with record
      SLASH_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Absolute_Location_Path_nonterminal1);
   procedure Evaluate(This : in Absolute_Location_Path_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Absolute_Location_Path_nonterminal2 is new Absolute_Location_Path_nonterminal with record
      SLASH_part : Parseable_Token_Ptr;
      Relative_Location_Path_part : Relative_Location_Path_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Absolute_Location_Path_nonterminal2);
   procedure Evaluate(This : in Absolute_Location_Path_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Absolute_Location_Path_nonterminal3 is new Absolute_Location_Path_nonterminal with record
      Abbreviated_Absolute_Location_Path_part : Abbreviated_Absolute_Location_Path_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Absolute_Location_Path_nonterminal3);
   procedure Evaluate(This : in Absolute_Location_Path_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Relative_Location_Path_nonterminal is abstract new Parseable with null record;
   type Relative_Location_Path_nonterminal1 is new Relative_Location_Path_nonterminal with record
      Step_part : Step_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Relative_Location_Path_nonterminal1);
   procedure Evaluate(This : in Relative_Location_Path_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Relative_Location_Path_nonterminal2 is new Relative_Location_Path_nonterminal with record
      Relative_Location_Path_part : Relative_Location_Path_nonterminal_Ptr;
      SLASH_part : Parseable_Token_Ptr;
      Step_part : Step_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Relative_Location_Path_nonterminal2);
   procedure Evaluate(This : in Relative_Location_Path_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Relative_Location_Path_nonterminal3 is new Relative_Location_Path_nonterminal with record
      Abbreviated_Relative_Location_Path_part : Abbreviated_Relative_Location_Path_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Relative_Location_Path_nonterminal3);
   procedure Evaluate(This : in Relative_Location_Path_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Step_nonterminal is abstract new Parseable with null record;
   type Step_nonterminal1 is new Step_nonterminal with record
      Step_Base_part : Step_Base_nonterminal_Ptr;
      Predicates_part : Predicates_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Step_nonterminal1);
   procedure Evaluate(This : in Step_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Step_nonterminal2 is new Step_nonterminal with record
      Abbreviated_Step_part : Abbreviated_Step_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Step_nonterminal2);
   procedure Evaluate(This : in Step_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Predicates_nonterminal is abstract new Parseable with null record;
   type Predicates_nonterminal1 is new Predicates_nonterminal with record
      null;
   end record;
   procedure Pathify(This : in out Predicates_nonterminal1);
   procedure Evaluate(This : in Predicates_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Predicates_nonterminal2 is new Predicates_nonterminal with record
      Predicates_part : Predicates_nonterminal_Ptr;
      Predicate_part : Predicate_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Predicates_nonterminal2);
   procedure Evaluate(This : in Predicates_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Step_Base_nonterminal is abstract new Parseable with null record;
   type Step_Base_nonterminal1 is new Step_Base_nonterminal with record
      Axis_Specifier_part : Axis_Specifier_nonterminal_Ptr;
      Node_Test_part : Node_Test_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Step_Base_nonterminal1);
   procedure Evaluate(This : in Step_Base_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Step_Base_nonterminal2 is new Step_Base_nonterminal with record
      Abbreviated_Step_Base_part : Abbreviated_Step_Base_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Step_Base_nonterminal2);
   procedure Evaluate(This : in Step_Base_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Abbreviated_Step_Base_nonterminal is abstract new Parseable with null record;
   type Abbreviated_Step_Base_nonterminal1 is new Abbreviated_Step_Base_nonterminal with record
      Node_Test_part : Node_Test_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Abbreviated_Step_Base_nonterminal1);
   procedure Evaluate(This : in Abbreviated_Step_Base_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Abbreviated_Step_Base_nonterminal2 is new Abbreviated_Step_Base_nonterminal with record
      AT_SIGN_part : Parseable_Token_Ptr;
      Node_Test_part : Node_Test_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Abbreviated_Step_Base_nonterminal2);
   procedure Evaluate(This : in Abbreviated_Step_Base_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Predicate_nonterminal is new Parseable with record
      L_BRACKET_part : Parseable_Token_Ptr;
      Predicate_Expr_part : Predicate_Expr_nonterminal_Ptr;
      R_BRACKET_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Predicate_nonterminal);
   procedure Evaluate(This : in Predicate_Nonterminal;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Axis_Specifier_nonterminal is new Parseable with record
      Axis_Name_part : Axis_Name_nonterminal_Ptr;
      DOUBLE_COLON_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Axis_Specifier_nonterminal);
   procedure Evaluate(This : in Axis_Specifier_Nonterminal;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Axis_Name_nonterminal is abstract new Parseable with null record;
   function Get_Axis_Name(This : in Axis_Name_Nonterminal)
     return Unbounded_String is abstract;

   type Axis_Name_nonterminal1 is new Axis_Name_nonterminal with record
      ANCESTOR_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal1) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal1);
   procedure Evaluate(This : in Axis_Name_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal2 is new Axis_Name_nonterminal with record
      ANCESTOR_OR_SELF_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal2) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal2);
   procedure Evaluate(This : in Axis_Name_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal3 is new Axis_Name_nonterminal with record
      ATTRIBUTE_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal3) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal3);
   procedure Evaluate(This : in Axis_Name_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal4 is new Axis_Name_nonterminal with record
      CHILD_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal4) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal4);
   procedure Evaluate(This : in Axis_Name_Nonterminal4;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal5 is new Axis_Name_nonterminal with record
      DESCENDANT_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal5) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal5);
   procedure Evaluate(This : in Axis_Name_Nonterminal5;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal6 is new Axis_Name_nonterminal with record
      DESCENDANT_OR_SELF_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal6) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal6);
   procedure Evaluate(This : in Axis_Name_Nonterminal6;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal7 is new Axis_Name_nonterminal with record
      FOLLOWING_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal7) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal7);
   procedure Evaluate(This : in Axis_Name_Nonterminal7;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal8 is new Axis_Name_nonterminal with record
      FOLLOWING_SIBLING_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal8) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal8);
   procedure Evaluate(This : in Axis_Name_Nonterminal8;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal9 is new Axis_Name_nonterminal with record
      NAMESPACE_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal9) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal9);
   procedure Evaluate(This : in Axis_Name_Nonterminal9;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal10 is new Axis_Name_nonterminal with record
      PARENT_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal10) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal10);
   procedure Evaluate(This : in Axis_Name_Nonterminal10;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal11 is new Axis_Name_nonterminal with record
      PRECEDING_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal11) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal11);
   procedure Evaluate(This : in Axis_Name_Nonterminal11;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal12 is new Axis_Name_nonterminal with record
      PRECEDING_SIBLING_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal12) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal12);
   procedure Evaluate(This : in Axis_Name_Nonterminal12;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Axis_Name_nonterminal13 is new Axis_Name_nonterminal with record
      SELF_part : Parseable_Token_Ptr;
   end record;
   function Get_Axis_Name(This : in Axis_Name_nonterminal13) return Unbounded_String;
   procedure Pathify(This : in out Axis_Name_nonterminal13);
   procedure Evaluate(This : in Axis_Name_Nonterminal13;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Node_Test_nonterminal is abstract new Parseable with null record;
   type Node_Test_nonterminal1 is new Node_Test_nonterminal with record
      Name_Test_part : Name_Test_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Node_Test_nonterminal1);
   procedure Evaluate(This : in Node_Test_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Node_Test_nonterminal2 is new Node_Test_nonterminal with record
      Node_Type_part : Node_Type_nonterminal_Ptr;
      L_PAREN_part : Parseable_Token_Ptr;
      R_PAREN_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Node_Test_nonterminal2);
   procedure Evaluate(This : in Node_Test_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Node_Test_nonterminal3 is new Node_Test_nonterminal with record
      PROCESSING_INSTRUCTION_part : Parseable_Token_Ptr;
      L_PAREN_part : Parseable_Token_Ptr;
      LITERAL_part : LITERAL_nonterminal_Ptr;
      R_PAREN_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Node_Test_nonterminal3);
   procedure Evaluate(This : in Node_Test_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Node_Type_nonterminal is abstract new Parseable with null record;
   function Get_Node_Type_Name(This : in Node_Type_Nonterminal)
     return Unbounded_String is abstract;

   type Node_Type_nonterminal1 is new Node_Type_nonterminal with record
      COMMENT_part : Parseable_Token_Ptr;
   end record;
   function Get_Node_Type_Name(This : in Node_Type_Nonterminal1) return Unbounded_String;
   procedure Pathify(This : in out Node_Type_nonterminal1);
   procedure Evaluate(This : in Node_Type_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Node_Type_nonterminal2 is new Node_Type_nonterminal with record
      TEXT_part : Parseable_Token_Ptr;
   end record;
   function Get_Node_Type_Name(This : in Node_Type_Nonterminal2) return Unbounded_String;
   procedure Pathify(This : in out Node_Type_nonterminal2);
   procedure Evaluate(This : in Node_Type_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Node_Type_nonterminal3 is new Node_Type_nonterminal with record
      PROCESSING_INSTRUCTION_part : Parseable_Token_Ptr;
   end record;
   function Get_Node_Type_Name(This : in Node_Type_Nonterminal3) return Unbounded_String;
   procedure Pathify(This : in out Node_Type_nonterminal3);
   procedure Evaluate(This : in Node_Type_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Node_Type_nonterminal4 is new Node_Type_nonterminal with record
      NODE_part : Parseable_Token_Ptr;
   end record;
   function Get_Node_Type_Name(This : in Node_Type_Nonterminal4) return Unbounded_String;
   procedure Pathify(This : in out Node_Type_nonterminal4);
   procedure Evaluate(This : in Node_Type_Nonterminal4;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Name_Test_nonterminal is abstract new Parseable with null record;
   type Name_Test_nonterminal1 is new Name_Test_nonterminal with record
      STAR_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Name_Test_nonterminal1);
   procedure Evaluate(This : in Name_Test_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Name_Test_nonterminal2 is new Name_Test_nonterminal with record
      QName_part : QName_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Name_Test_nonterminal2);
   procedure Evaluate(This : in Name_Test_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Name_Test_nonterminal3 is new Name_Test_nonterminal with record
      NCNAME_Or_ID_part : NCNAME_Or_ID_nonterminal_Ptr;
      COLON_part : Parseable_Token_Ptr;
      STAR_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Name_Test_nonterminal3);
   procedure Evaluate(This : in Name_Test_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type QName_nonterminal is abstract new Parseable with null record;
   type QName_nonterminal1 is new QName_nonterminal with record
      NCNAME_Or_ID_part : NCNAME_Or_ID_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out QName_nonterminal1);
   procedure Evaluate(This : in QName_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type QName_nonterminal2 is new QName_nonterminal with record
      NCNAME_Or_ID_part1 : NCNAME_Or_ID_nonterminal_Ptr;
      COLON_part : Parseable_Token_Ptr;
      NCNAME_Or_ID_part2 : NCNAME_Or_ID_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out QName_nonterminal2);
   procedure Evaluate(This : in QName_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type NCNAME_Or_ID_nonterminal is abstract new Parseable with null record;
   type NCNAME_Or_ID_nonterminal1 is new NCNAME_Or_ID_nonterminal with record
      NCNAME_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal1);
   procedure Evaluate(This : in NCNAME_Or_ID_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type NCNAME_Or_ID_nonterminal2 is new NCNAME_Or_ID_nonterminal with record
      Axis_Name_part : Axis_Name_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal2);
   procedure Evaluate(This : in NCNAME_Or_ID_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type NCNAME_Or_ID_nonterminal3 is new NCNAME_Or_ID_nonterminal with record
      Node_Type_part : Node_Type_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal3);
   procedure Evaluate(This : in NCNAME_Or_ID_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type NCNAME_Or_ID_nonterminal4 is new NCNAME_Or_ID_nonterminal with record
      AND_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal4);
   procedure Evaluate(This : in NCNAME_Or_ID_Nonterminal4;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type NCNAME_Or_ID_nonterminal5 is new NCNAME_Or_ID_nonterminal with record
      OR_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal5);
   procedure Evaluate(This : in NCNAME_Or_ID_Nonterminal5;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type NCNAME_Or_ID_nonterminal6 is new NCNAME_Or_ID_nonterminal with record
      MOD_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal6);
   procedure Evaluate(This : in NCNAME_Or_ID_Nonterminal6;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type NCNAME_Or_ID_nonterminal7 is new NCNAME_Or_ID_nonterminal with record
      DIV_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal7);
   procedure Evaluate(This : in NCNAME_Or_ID_Nonterminal7;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Predicate_Expr_nonterminal is new Parseable with record
      Expr_part : Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Predicate_Expr_nonterminal);
   procedure Evaluate(This : in Predicate_Expr_Nonterminal;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Abbreviated_Absolute_Location_Path_nonterminal is new Parseable with record
      DOUBLE_SLASH_part : Parseable_Token_Ptr;
      Relative_Location_Path_part : Relative_Location_Path_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Abbreviated_Absolute_Location_Path_nonterminal);
   procedure Evaluate(This : in Abbreviated_Absolute_Location_Path_Nonterminal;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Abbreviated_Relative_Location_Path_nonterminal is new Parseable with record
      Relative_Location_Path_part : Relative_Location_Path_nonterminal_Ptr;
      DOUBLE_SLASH_part : Parseable_Token_Ptr;
      Step_part : Step_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Abbreviated_Relative_Location_Path_nonterminal);
   procedure Evaluate(This : in Abbreviated_Relative_Location_Path_Nonterminal;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Abbreviated_Step_nonterminal is abstract new Parseable with null record;
   type Abbreviated_Step_nonterminal1 is new Abbreviated_Step_nonterminal with record
      DOT_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Abbreviated_Step_nonterminal1);
   procedure Evaluate(This : in Abbreviated_Step_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Abbreviated_Step_nonterminal2 is new Abbreviated_Step_nonterminal with record
      DOUBLE_DOT_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Abbreviated_Step_nonterminal2);
   procedure Evaluate(This : in Abbreviated_Step_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Expr_nonterminal is new Parseable with record
      Or_Expr_part : Or_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Expr_nonterminal);
   procedure Evaluate(This : in Expr_Nonterminal;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);



--     procedure Evaluate (This         : in     Expr_Nonterminal;
--                         Context_Node : in     Node_Items;
--                         Value        :    out Expression_Values);

   type Primary_Expr_nonterminal is abstract new Parseable with null record;
   type Primary_Expr_nonterminal1 is new Primary_Expr_nonterminal with record
      Variable_Reference_part : Variable_Reference_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Primary_Expr_nonterminal1);
   procedure Evaluate(This : in Primary_Expr_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Primary_Expr_nonterminal2 is new Primary_Expr_nonterminal with record
      L_PAREN_part : Parseable_Token_Ptr;
      Expr_part : Expr_nonterminal_Ptr;
      R_PAREN_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Primary_Expr_nonterminal2);
   procedure Evaluate(This : in Primary_Expr_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Primary_Expr_nonterminal3 is new Primary_Expr_nonterminal with record
      LITERAL_part : LITERAL_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Primary_Expr_nonterminal3);
   procedure Evaluate(This : in Primary_Expr_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Primary_Expr_nonterminal4 is new Primary_Expr_nonterminal with record
      Number_part : Number_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Primary_Expr_nonterminal4);
   procedure Evaluate(This : in Primary_Expr_Nonterminal4;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Primary_Expr_nonterminal5 is new Primary_Expr_nonterminal with record
      Function_Call_part : Function_Call_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Primary_Expr_nonterminal5);
   procedure Evaluate(This : in Primary_Expr_Nonterminal5;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Variable_Reference_nonterminal is new Parseable with record
      DOLLAR_part : Parseable_Token_Ptr;
      QName_part : QName_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Variable_Reference_nonterminal);
   procedure Evaluate(This : in Variable_Reference_Nonterminal;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Function_Call_nonterminal is new Parseable with record
      Function_Name_part : Function_Name_nonterminal_Ptr;
      L_PAREN_part : Parseable_Token_Ptr;
      Arguments_part : Arguments_nonterminal_Ptr;
      R_PAREN_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Function_Call_nonterminal);
   procedure Evaluate(This : in Function_Call_Nonterminal;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Arguments_nonterminal is abstract new Parseable with null record;
   type Arguments_nonterminal1 is new Arguments_nonterminal with record
      null;
   end record;
   procedure Pathify(This : in out Arguments_nonterminal1);
   procedure Evaluate(This : in Arguments_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Arguments_nonterminal2 is new Arguments_nonterminal with record
      Argument_part : Argument_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Arguments_nonterminal2);
   procedure Evaluate(This : in Arguments_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Arguments_nonterminal3 is new Arguments_nonterminal with record
      Arguments_part : Arguments_nonterminal_Ptr;
      COMMA_part : Parseable_Token_Ptr;
      Argument_part : Argument_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Arguments_nonterminal3);
   procedure Evaluate(This : in Arguments_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Argument_nonterminal is new Parseable with record
      Expr_part : Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Argument_nonterminal);
   procedure Evaluate(This : in Argument_Nonterminal;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Function_Name_nonterminal is new Parseable with record
      QName_part : QName_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Function_Name_nonterminal);
   procedure Evaluate(This : in Function_Name_Nonterminal;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Union_Expr_nonterminal is abstract new Parseable with null record;
   type Union_Expr_nonterminal1 is new Union_Expr_nonterminal with record
      Path_Expr_part : Path_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Union_Expr_nonterminal1);
   procedure Evaluate(This : in Union_Expr_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Union_Expr_nonterminal2 is new Union_Expr_nonterminal with record
      Union_Expr_part : Union_Expr_nonterminal_Ptr;
      V_BAR_part : Parseable_Token_Ptr;
      Path_Expr_part : Path_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Union_Expr_nonterminal2);
   procedure Evaluate(This : in Union_Expr_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Path_Expr_nonterminal is abstract new Parseable with null record;
   type Path_Expr_nonterminal1 is new Path_Expr_nonterminal with record
      Location_Path_part : Location_Path_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Path_Expr_nonterminal1);
   procedure Evaluate(This : in Path_Expr_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Path_Expr_nonterminal2 is new Path_Expr_nonterminal with record
      Filter_Expr_part : Filter_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Path_Expr_nonterminal2);
   procedure Evaluate(This : in Path_Expr_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Path_Expr_nonterminal3 is new Path_Expr_nonterminal with record
      Filter_Expr_part : Filter_Expr_nonterminal_Ptr;
      SLASH_part : Parseable_Token_Ptr;
      Relative_Location_Path_part : Relative_Location_Path_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Path_Expr_nonterminal3);
   procedure Evaluate(This : in Path_Expr_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Path_Expr_nonterminal4 is new Path_Expr_nonterminal with record
      Filter_Expr_part : Filter_Expr_nonterminal_Ptr;
      DOUBLE_SLASH_part : Parseable_Token_Ptr;
      Relative_Location_Path_part : Relative_Location_Path_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Path_Expr_nonterminal4);
   procedure Evaluate(This : in Path_Expr_Nonterminal4;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Filter_Expr_nonterminal is abstract new Parseable with null record;
   type Filter_Expr_nonterminal1 is new Filter_Expr_nonterminal with record
      Primary_Expr_part : Primary_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Filter_Expr_nonterminal1);
   procedure Evaluate(This : in Filter_Expr_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Filter_Expr_nonterminal2 is new Filter_Expr_nonterminal with record
      Filter_Expr_part : Filter_Expr_nonterminal_Ptr;
      Predicate_part : Predicate_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Filter_Expr_nonterminal2);
   procedure Evaluate(This : in Filter_Expr_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Or_Expr_nonterminal is abstract new Parseable with null record;
   type Or_Expr_nonterminal1 is new Or_Expr_nonterminal with record
      And_Expr_part : And_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Or_Expr_nonterminal1);
   procedure Evaluate(This : in Or_Expr_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Or_Expr_nonterminal2 is new Or_Expr_nonterminal with record
      Or_Expr_part : Or_Expr_nonterminal_Ptr;
      OR_part : Parseable_Token_Ptr;
      And_Expr_part : And_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Or_Expr_nonterminal2);
   procedure Evaluate(This : in Or_Expr_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type And_Expr_nonterminal is abstract new Parseable with null record;
   type And_Expr_nonterminal1 is new And_Expr_nonterminal with record
      Equality_Expr_part : Equality_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out And_Expr_nonterminal1);
   procedure Evaluate(This : in And_Expr_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type And_Expr_nonterminal2 is new And_Expr_nonterminal with record
      And_Expr_part : And_Expr_nonterminal_Ptr;
      AND_part : Parseable_Token_Ptr;
      Equality_Expr_part : Equality_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out And_Expr_nonterminal2);
   procedure Evaluate(This : in And_Expr_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Equality_Expr_nonterminal is abstract new Parseable with null record;
   type Equality_Expr_nonterminal1 is new Equality_Expr_nonterminal with record
      Relational_Expr_part : Relational_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Equality_Expr_nonterminal1);
   procedure Evaluate(This : in Equality_Expr_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Equality_Expr_nonterminal2 is new Equality_Expr_nonterminal with record
      Equality_Expr_part : Equality_Expr_nonterminal_Ptr;
      EQ_part : Parseable_Token_Ptr;
      Relational_Expr_part : Relational_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Equality_Expr_nonterminal2);
   procedure Evaluate(This : in Equality_Expr_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Equality_Expr_nonterminal3 is new Equality_Expr_nonterminal with record
      Equality_Expr_part : Equality_Expr_nonterminal_Ptr;
      NE_part : Parseable_Token_Ptr;
      Relational_Expr_part : Relational_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Equality_Expr_nonterminal3);
   procedure Evaluate(This : in Equality_Expr_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Relational_Expr_nonterminal is abstract new Parseable with null record;
   type Relational_Expr_nonterminal1 is new Relational_Expr_nonterminal with record
      Additive_Expr_part : Additive_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Relational_Expr_nonterminal1);
   procedure Evaluate(This : in Relational_Expr_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Relational_Expr_nonterminal2 is new Relational_Expr_nonterminal with record
      Relational_Expr_part : Relational_Expr_nonterminal_Ptr;
      LT_part : Parseable_Token_Ptr;
      Additive_Expr_part : Additive_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Relational_Expr_nonterminal2);
   procedure Evaluate(This : in Relational_Expr_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Relational_Expr_nonterminal3 is new Relational_Expr_nonterminal with record
      Relational_Expr_part : Relational_Expr_nonterminal_Ptr;
      GT_part : Parseable_Token_Ptr;
      Additive_Expr_part : Additive_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Relational_Expr_nonterminal3);
   procedure Evaluate(This : in Relational_Expr_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Relational_Expr_nonterminal4 is new Relational_Expr_nonterminal with record
      Relational_Expr_part : Relational_Expr_nonterminal_Ptr;
      LE_part : Parseable_Token_Ptr;
      Additive_Expr_part : Additive_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Relational_Expr_nonterminal4);
   procedure Evaluate(This : in Relational_Expr_Nonterminal4;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Relational_Expr_nonterminal5 is new Relational_Expr_nonterminal with record
      Relational_Expr_part : Relational_Expr_nonterminal_Ptr;
      GE_part : Parseable_Token_Ptr;
      Additive_Expr_part : Additive_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Relational_Expr_nonterminal5);
   procedure Evaluate(This : in Relational_Expr_Nonterminal5;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Additive_Expr_nonterminal is abstract new Parseable with null record;
   type Additive_Expr_nonterminal1 is new Additive_Expr_nonterminal with record
      Multiplicative_Expr_part : Multiplicative_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Additive_Expr_nonterminal1);
   procedure Evaluate(This : in Additive_Expr_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Additive_Expr_nonterminal2 is new Additive_Expr_nonterminal with record
      Additive_Expr_part : Additive_Expr_nonterminal_Ptr;
      PLUS_part : Parseable_Token_Ptr;
      Multiplicative_Expr_part : Multiplicative_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Additive_Expr_nonterminal2);
   procedure Evaluate(This : in Additive_Expr_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Additive_Expr_nonterminal3 is new Additive_Expr_nonterminal with record
      Additive_Expr_part : Additive_Expr_nonterminal_Ptr;
      MINUS_part : Parseable_Token_Ptr;
      Multiplicative_Expr_part : Multiplicative_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Additive_Expr_nonterminal3);
   procedure Evaluate(This : in Additive_Expr_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Multiplicative_Expr_nonterminal is abstract new Parseable with null record;
   type Multiplicative_Expr_nonterminal1 is new Multiplicative_Expr_nonterminal with record
      Unary_Expr_part : Unary_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Multiplicative_Expr_nonterminal1);
   procedure Evaluate(This : in Multiplicative_Expr_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Multiplicative_Expr_nonterminal2 is new Multiplicative_Expr_nonterminal with record
      Multiplicative_Expr_part : Multiplicative_Expr_nonterminal_Ptr;
      STAR_part : Parseable_Token_Ptr;
      Unary_Expr_part : Unary_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Multiplicative_Expr_nonterminal2);
   procedure Evaluate(This : in Multiplicative_Expr_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Multiplicative_Expr_nonterminal3 is new Multiplicative_Expr_nonterminal with record
      Multiplicative_Expr_part : Multiplicative_Expr_nonterminal_Ptr;
      DIV_part : Parseable_Token_Ptr;
      Unary_Expr_part : Unary_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Multiplicative_Expr_nonterminal3);
   procedure Evaluate(This : in Multiplicative_Expr_Nonterminal3;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Multiplicative_Expr_nonterminal4 is new Multiplicative_Expr_nonterminal with record
      Multiplicative_Expr_part : Multiplicative_Expr_nonterminal_Ptr;
      MOD_part : Parseable_Token_Ptr;
      Unary_Expr_part : Unary_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Multiplicative_Expr_nonterminal4);
   procedure Evaluate(This : in Multiplicative_Expr_Nonterminal4;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Unary_Expr_nonterminal is abstract new Parseable with null record;
   type Unary_Expr_nonterminal1 is new Unary_Expr_nonterminal with record
      Union_Expr_part : Union_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Unary_Expr_nonterminal1);
   procedure Evaluate(This : in Unary_Expr_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Unary_Expr_nonterminal2 is new Unary_Expr_nonterminal with record
      MINUS_part : Parseable_Token_Ptr;
      Unary_Expr_part : Unary_Expr_nonterminal_Ptr;
   end record;
   procedure Pathify(This : in out Unary_Expr_nonterminal2);
   procedure Evaluate(This : in Unary_Expr_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Number_nonterminal is abstract new Parseable with null record;
   type Number_nonterminal1 is new Number_nonterminal with record
      INTEGER_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Number_nonterminal1);
   procedure Evaluate(This : in Number_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Number_nonterminal2 is new Number_nonterminal with record
      DECIMAL_LITERAL_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Number_nonterminal2);
   procedure Evaluate(This : in Number_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   type Literal_nonterminal is abstract new Parseable with null record;
   type Literal_nonterminal1 is new Literal_nonterminal with record
      DQ_LITERAL_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Literal_nonterminal1);
   procedure Evaluate(This : in Literal_Nonterminal1;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);
   type Literal_nonterminal2 is new Literal_nonterminal with record
      SQ_LITERAL_part : Parseable_Token_Ptr;
   end record;
   procedure Pathify(This : in out Literal_nonterminal2);
   procedure Evaluate(This : in Literal_Nonterminal2;
                      Context_Node : in     Node_Items;
                      Value : out Expression_Values);

   procedure Reset;

end xia_parser_Model;
