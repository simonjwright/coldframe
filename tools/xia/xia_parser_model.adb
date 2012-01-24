-----------------------------------------------------------------------
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

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;
with Ada.Tags;

with Dom.Core.Append_Node;
with Dom.Core.Nodes;

with McKae.XML.XPath.Expressions;
with Mckae.XML.XPath.Locations;
with Mckae.XML.XPath.Predicates;
with Mckae.XML.XPath.Xia;

with Text_IO; use Text_IO;

package body xia_parser_Model is

   use Mckae.XML.Xpath;
   use McKae.XML.XPath.Expressions;
   use Mckae.XML.Xpath.Locations;

   ----------------------------------------------------------

   Star : constant Unbounded_String := To_Unbounded_String("*");

   ----------------------------------------------------------

   function "+"(S : String) return Unbounded_String
     renames To_Unbounded_String;

   ----------------------------------------------------------

   -- Location step under construction
   Location_Step : Location_Steps;

   -- Rely on the initialization to set establish this as an empty step
   Empty_Location_Step : Location_Steps;

   ----------------------------------------------------------

   function Evaluate_Arguments(Arguments_Part : Arguments_Nonterminal_Ptr;
                               Context_Node   : Node_Items)
     return Argument_List;

   ----------------------------------------------------------

   procedure Reset is
   begin
      Predicates.Release(Location_Step.Location_Predicates);
      Location_Step := Empty_Location_Step;
   end Reset;

   ----------------------------------------------------------

   function Is_In (N  : Dom.Core.Node;
                   Nl : Dom.Core.Node_List) return Boolean is
      use type Dom.Core.Node;
   begin
      for I in 0 .. Dom.Core.Nodes.Length(Nl) - 1 loop
         if N = Dom.Core.Nodes.Item(Nl, I) then
            return True;
         end if;
      end loop;
      return False;
   end Is_In;

   ----------------------------------------------------------

   function Get_Literal(Literal_Part : Literal_Nonterminal_Ptr)
                       return Unbounded_String is
      S : Unbounded_String;

      use type Ada.Tags.Tag;
   begin
      if Literal_Part.all'Tag = Literal_Nonterminal1'Tag then
         declare
            Quoted_String : constant String :=
              Literal_Nonterminal1(Literal_Part.all).DQ_Literal_Part.Token_String.all;
         begin
            S := To_Unbounded_String(Quoted_String(2 .. Quoted_String'Last - 1));
         end;
      elsif Literal_Part.all'Tag = Literal_Nonterminal2'Tag then
         declare
            Quoted_String : constant String :=
              Literal_Nonterminal2(Literal_Part.all).SQ_Literal_Part.Token_String.all;
         begin
            S := To_Unbounded_String(Quoted_String(2 .. Quoted_String'Last - 1));
         end;
      else
         pragma Assert(False);
         S := Null_Unbounded_String;
      end if;
      return S;
   end Get_Literal;

   ----------------------------------------------------------

   procedure Get_Number (Number_Part : in     Number_Nonterminal_Ptr;
                         Value       :    out Long_Float;
                         Special     :    out Special_Number_Values) is

      use type Ada.Tags.Tag;

   begin
      if Number_Part.all'Tag = Number_Nonterminal1'Tag then
         Value := Long_Float(Natural'Value
                             (Number_Nonterminal1(Number_Part.all).Integer_Part.Token_String.all));
         Special := Normal;

      elsif Number_Part.all'Tag = Number_Nonterminal2'Tag then
         Value := Long_Float'Value
           (Number_Nonterminal2(Number_Part.all).Decimal_Literal_Part.Token_String.all);
         Special := Normal;

      else
         pragma Assert(False);
         Value := 0.0;
         Special := NaN;
      end if;

   exception
      when others =>
         Value := 0.0;
         Special := NaN;
   end Get_Number;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal1)
                         return Unbounded_String is
   begin
      return +This.Ancestor_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal2)
                         return Unbounded_String is
   begin
      return +This.Ancestor_Or_Self_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal3)
                         return Unbounded_String is
   begin
      return +This.Attribute_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal4)
                         return Unbounded_String is
   begin
      return +This.Child_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal5)
                         return Unbounded_String is
   begin
      return +This.Descendant_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal6)
                         return Unbounded_String is
   begin
      return +This.Descendant_Or_Self_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal7)
                         return Unbounded_String is
   begin
      return +This.Following_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal8)
                         return Unbounded_String is
   begin
      return +This.Following_Sibling_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal9)
                         return Unbounded_String is
   begin
      return +This.Namespace_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal10)
                         return Unbounded_String is
   begin
      return +This.Parent_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal11)
                         return Unbounded_String is
   begin
      return +This.Preceding_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal12)
                         return Unbounded_String is
   begin
      return +This.Preceding_Sibling_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Axis_Name(This : Axis_Name_Nonterminal13)
                         return Unbounded_String is
   begin
      return +This.Self_Part.Token_String.all;
   end Get_Axis_Name;

   ----------------------------------------------------------

   function Get_Node_Type_Name(This : Node_Type_Nonterminal1)
                         return Unbounded_String is
   begin
      return +This.Comment_Part.Token_String.all;
   end Get_Node_Type_Name;

   ----------------------------------------------------------

   function Get_Node_Type_Name(This : Node_Type_Nonterminal2)
                         return Unbounded_String is
   begin
      return +This.Text_Part.Token_String.all;
   end Get_Node_Type_Name;

   ----------------------------------------------------------

   function Get_Node_Type_Name(This : Node_Type_Nonterminal3)
                         return Unbounded_String is
   begin
      return +This.Processing_Instruction_Part.Token_String.all;
   end Get_Node_Type_Name;

   ----------------------------------------------------------

   function Get_Node_Type_Name(This : Node_Type_Nonterminal4)
                         return Unbounded_String is
   begin
      return +This.Node_Part.Token_String.all;
   end Get_Node_Type_Name;

   ----------------------------------------------------------

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Parseable_Token) is
   begin
      null;
   end Pathify;

   ----------------------------------------------------------
   --
   -- These three Pathify procedures are the only ones through which
   --  the parse tree traversal will commence; two for different forms
   --  of relative paths, and one for absolute paths.  The working
   --  step definition is cleared, filled out by invoking the parse
   --  tree, and then added to the processed location path.

   procedure Pathify (This : in out Location_Path_nonterminal1) is
   begin
      Pathify(This.Relative_Location_Path_Part.all);
      Add(Location_Step);
      Reset;
   end Pathify;

   procedure Pathify (This : in out Location_Path_nonterminal2) is
   begin
      Pathify(This.Absolute_Location_Path_Part.all);
      Add(Location_Step);
      Reset;
   end Pathify;

   procedure Pathify (This : in out Relative_Location_Path_nonterminal2) is
   begin
      -- An initial relative location is included here, process it,
      --  add the step, then work on the next step part
      Pathify(This.Relative_Location_Path_Part.all);
      Add(Location_Step);
      Reset;
      Pathify(This.Step_Part.all);
   end Pathify;

   --
   ----------------------------------------------------------

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Absolute_Location_Path_nonterminal1) is
   begin
      Location_Step := (Axis                => Self_Axis,
                        Node_Test           => (Node_Test => Node_Node_Test,
                                                Name      => Null_Unbounded_String),
                        Location_Predicates => Predicates.Null_Predicate,
                        Output_Step         => False);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Absolute_Location_Path_nonterminal2) is
   begin
      Pathify(This.Relative_Location_Path_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Absolute_Location_Path_nonterminal3) is
   begin
      Pathify(This.Abbreviated_Absolute_Location_Path_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Relative_Location_Path_nonterminal1) is
   begin
      Pathify(This.Step_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Relative_Location_Path_nonterminal3) is
   begin
      Pathify(This.Abbreviated_Relative_Location_Path_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Step_nonterminal1) is
   begin
      Pathify(This.Step_Base_Part.all);
      Pathify(This.Predicates_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Step_nonterminal2) is
   begin
      Pathify(This.Abbreviated_Step_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Predicates_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Predicates_nonterminal2) is
   begin
      Pathify(This.Predicates_Part.all);
      Pathify(This.Predicate_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Step_Base_nonterminal1) is
   begin
      Pathify(This.Axis_Specifier_Part.all);
      Pathify(This.Node_Test_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Step_Base_nonterminal2) is
   begin
      Pathify(This.Abbreviated_Step_Base_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Abbreviated_Step_Base_nonterminal1) is
   begin
      Location_Step.Axis := Child_Axis;
      Pathify(This.Node_Test_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Abbreviated_Step_Base_nonterminal2) is
   begin
      Location_Step.Axis := Attribute_Axis;
      Pathify(This.Node_Test_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Predicate_nonterminal) is
   begin
      -- This is the root of the subtree of the  predicate expression
      Predicates.Add_Predicate_Parse
        (Location_Step.Location_Predicates,
         This.Predicate_Expr_Part.all'Access);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Specifier_nonterminal) is
   begin
      Pathify(This.Axis_Name_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal1) is
   begin
      Location_Step.Axis := Ancestor_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal2) is
   begin
      Location_Step.Axis := Ancestor_Or_Self_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal3) is
   begin
      Location_Step.Axis := Attribute_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal4) is
   begin
      Location_Step.Axis := Child_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal5) is
   begin
      Location_Step.Axis := Descendant_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal6) is
   begin
      Location_Step.Axis := Descendant_Or_Self_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal7) is
   begin
      Location_Step.Axis := Following_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal8) is
   begin
      Location_Step.Axis := Following_Sibling_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal9) is
   begin
      Location_Step.Axis := Namespace_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal10) is
   begin
      Location_Step.Axis := Parent_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal11) is
   begin
      Location_Step.Axis := Preceding_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal12) is
   begin
      Location_Step.Axis := Preceding_Sibling_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Axis_Name_nonterminal13) is
   begin
      Location_Step.Axis := Self_Axis;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Node_Test_nonterminal1) is
   begin
      Location_Step.Node_Test := (Node_Test => NCName_Node_Test,
                                  Name      => Null_Unbounded_String); -- Set via Name_Test_Part
      Pathify(This.Name_Test_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Node_Test_nonterminal2) is
   begin
      Pathify(This.Node_Type_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Node_Test_nonterminal3) is

      S : Unbounded_String := Get_Literal(This.Literal_Part);

   begin
      Location_Step.Node_Test := (Node_Test => Processing_Instruction_Node_Test,
                                  Name      => S);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Node_Type_nonterminal1) is
   begin
      Location_Step.Node_Test := (Node_Test => Comment_Node_Test,
                                  Name      => Null_Unbounded_String);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Node_Type_nonterminal2) is
   begin
      Location_Step.Node_Test := (Node_Test => Text_Node_Test,
                                  Name      => Null_Unbounded_String);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Node_Type_nonterminal3) is
   begin
      Location_Step.Node_Test := (Node_Test => Processing_Instruction_Node_Test,
                                  Name      => Null_Unbounded_String);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Node_Type_nonterminal4) is
   begin
      Location_Step.Node_Test := (Node_Test => Node_Node_Test,
                                  Name      => Null_Unbounded_String);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Name_Test_nonterminal1) is
   begin
      Location_Step.Node_Test.Name := Star;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Name_Test_nonterminal2) is
   begin
      Pathify(This.Qname_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Name_Test_nonterminal3) is
   begin
      Pathify(This.NCName_Or_ID_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out QName_nonterminal1) is
   begin
      Pathify(This.NCName_Or_ID_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out QName_nonterminal2) is
   begin
      Location_Step.Node_Test := (Node_Test => QName_Node_Test,
                                  Name      => To_Unbounded_String
                                  (NCNAME_Or_ID_Nonterminal1
                                   (This.NCName_Or_ID_Part2.all).NCName_Part.Token_String.all),
                                  Prefix    => To_Unbounded_String
                                  (NCNAME_Or_ID_Nonterminal1
                                   (This.NCName_Or_ID_Part1.all).NCName_Part.Token_String.all));
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal1) is
   begin
      Location_Step.Node_Test := (Node_Test => QName_Node_Test,
                                  Name      => To_Unbounded_String(This.NCName_Part.Token_String.all),
                                  Prefix    => Null_Unbounded_String);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal2) is
   begin
      Location_Step.Node_Test := (Node_Test => QName_Node_Test,
                                  Name      => Get_Axis_Name(This.Axis_Name_Part.all),
                                  Prefix    => Null_Unbounded_String);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal3) is
   begin
      Location_Step.Node_Test := (Node_Test => QName_Node_Test,
                                  Name      => Get_Node_Type_Name(This.Node_Type_Part.all),
                                  Prefix    => Null_Unbounded_String);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal4) is
   begin
      Location_Step.Node_Test := (Node_Test => QName_Node_Test,
                                  Name      => To_Unbounded_String(This.And_Part.Token_String.all),
                                  Prefix    => Null_Unbounded_String);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal5) is
   begin
      Location_Step.Node_Test := (Node_Test => QName_Node_Test,
                                  Name      => To_Unbounded_String(This.Or_Part.Token_String.all),
                                  Prefix    => Null_Unbounded_String);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal6) is
   begin
      Location_Step.Node_Test := (Node_Test => QName_Node_Test,
                                  Name      => To_Unbounded_String(This.Mod_Part.Token_String.all),
                                  Prefix    => Null_Unbounded_String);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify(This : in out NCNAME_Or_ID_nonterminal7) is
   begin
      Location_Step.Node_Test := (Node_Test => QName_Node_Test,
                                  Name      => To_Unbounded_String(This.Div_Part.Token_String.All),
                                  Prefix    => Null_Unbounded_String);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Predicate_Expr_nonterminal) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify
     (This : in out Abbreviated_Absolute_Location_Path_nonterminal)
   is
   begin
      -- The "//" part
      Add((Axis                => Descendant_Or_Self_Axis,
           Node_Test           => (Node_Test => Node_Node_Test,
                                   Name      => Star),
           Location_Predicates => Predicates.Null_Predicate,
           Output_Step         => False));
      Reset;
      Pathify(This.Relative_Location_Path_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify
     (This : in out Abbreviated_Relative_Location_Path_nonterminal)
   is
   begin
      Pathify(This.Relative_Location_Path_Part.all);
      Add(Location_Step);
      Add((Axis                => Descendant_Or_Self_Axis,
           Node_Test           => (Node_Test => Node_Node_Test,
                                   Name      => Star),
           Location_Predicates => Predicates.Null_Predicate,
           Output_Step         => False));
      Reset;
      Pathify(This.Step_Part.all);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Abbreviated_Step_nonterminal1) is
   begin
      Location_Step := (Axis                => Self_Axis,
                        Node_Test           => (Node_Test => Node_Node_Test,
                                                Name      => Star),
                        Location_Predicates => Predicates.Null_Predicate,
                        Output_Step         => False);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Abbreviated_Step_nonterminal2) is
   begin
      Location_Step := (Axis                => Parent_Axis,
                        Node_Test           => (Node_Test => Node_Node_Test,
                                                Name      => Star),
                        Location_Predicates => Predicates.Null_Predicate,
                        Output_Step         => False);
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Expr_nonterminal) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Primary_Expr_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Primary_Expr_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Primary_Expr_nonterminal3) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Primary_Expr_nonterminal4) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Primary_Expr_nonterminal5) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Variable_Reference_nonterminal) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Function_Call_nonterminal) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Arguments_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Arguments_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Arguments_nonterminal3) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Argument_nonterminal) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Function_Name_nonterminal) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Union_Expr_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Union_Expr_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Path_Expr_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Path_Expr_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Path_Expr_nonterminal3) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Path_Expr_nonterminal4) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Filter_Expr_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Filter_Expr_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Or_Expr_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Or_Expr_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out And_Expr_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out And_Expr_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Equality_Expr_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Equality_Expr_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Equality_Expr_nonterminal3) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Relational_Expr_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Relational_Expr_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Relational_Expr_nonterminal3) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Relational_Expr_nonterminal4) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Relational_Expr_nonterminal5) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Additive_Expr_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Additive_Expr_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Additive_Expr_nonterminal3) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Multiplicative_Expr_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Multiplicative_Expr_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Multiplicative_Expr_nonterminal3) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Multiplicative_Expr_nonterminal4) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Unary_Expr_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Unary_Expr_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Number_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Number_nonterminal2) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Literal_nonterminal1) is
   begin
      null;
   end Pathify;

   -------------
   -- Pathify --
   -------------

   procedure Pathify (This : in out Literal_nonterminal2) is
   begin
      null;
   end Pathify;

   ----------------------------------------------------------
   ----------------------------------------------------------
   ----------------------------------------------------------
   ----------------------------------------------------------

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Parseable_Token;
                       Context_Node : in     Node_Items;
                       Value : out Expression_Values) is
   begin
      Put_Line("Evaluating Token: " & " Parseable_Token");
      Value := (As_Boolean, True);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Location_Path_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Predicate_Path_Nodes : Dom.Core.Node_List;
   begin
      Value := (As_Expr_Text, Null_Unbounded_String);
      Evaluate(This.Relative_Location_Path_Part.all, Context_Node, Value);
      Predicate_Path_Nodes := Xia.Xpath_Query(Context_Node.N, To_String(Value.S));
      Value := (As_Node_List, Predicate_Path_Nodes);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Location_Path_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Predicate_Path_Nodes : Dom.Core.Node_List;
   begin
      Value := (As_Expr_Text, Null_Unbounded_String);
      Evaluate(This.Absolute_Location_Path_Part.all, Context_Node, Value);
      Predicate_Path_Nodes := Xia.Xpath_Query(Context_Node.N, To_String(Value.S));
      Value := (As_Node_List, Predicate_Path_Nodes);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Relative_Location_Path_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Step_Value : Expression_Values(As_Expr_Text);
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Relative_Location_Path_Part.all, Context_Node, Value);
      Evaluate(This.Step_Part.all, Context_Node, Step_Value);
      Append(Value.S, "/" & Step_Value.S);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Absolute_Location_Path_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"/";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Absolute_Location_Path_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Relative_Location_Path_Part.all, Context_Node, Value);
      Value.S := "/" & Value.S;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Absolute_Location_Path_Nonterminal3;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Abbreviated_Absolute_Location_Path_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Relative_Location_Path_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Step_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Relative_Location_Path_Nonterminal3;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Abbreviated_Relative_Location_Path_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Step_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Predicate_Value : Expression_Values(As_Expr_Text);
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Step_Base_Part.all, Context_Node, Value);
      Evaluate(This.Predicates_Part.all, Context_Node, Predicate_Value);
      Append(Value.S, Predicate_Value.S);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Step_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Abbreviated_Step_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Predicates_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      pragma Warnings(Off, Value);
   begin
      -- There is no predicate.
      pragma Assert(Value.Value_Type = As_Expr_Text);
      null;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Predicates_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Predicate_Value : Expression_Values(As_Expr_Text);
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Predicates_Part.all, Context_Node, Value);
      Evaluate(This.Predicate_Part.all, Context_Node, Predicate_Value);
      Append(Value.S, Predicate_Value.S);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Step_Base_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Node_Test_Value : Expression_Values(As_Expr_Text);
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Axis_Specifier_Part.all, Context_Node, Value);
      Evaluate(This.Node_Test_Part.all, Context_Node, Node_Test_Value);
      Append(Value.S, Node_Test_Value.S);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Step_Base_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Abbreviated_Step_Base_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Abbreviated_Step_Base_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Node_Test_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Abbreviated_Step_Base_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Node_Test_Part.all, Context_Node, Value);
      Value.S := "@" & Value.S;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Predicate_Nonterminal;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Predicate_Expr_Part.all, Context_Node, Value);
      Value.S := "[" & Value.S & "]";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Axis_Specifier_Nonterminal;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Axis_Name_Part.all, Context_Node, Value);
      Append(Value.S, "::");
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal1;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"ancestor";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal2;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"ancestor-or-self";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal3;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"attribute";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal4;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"child";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal5;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"descendant";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal6;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"descendant-or-self";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal7;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"following";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal8;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"following-sibling";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal9;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"namespace";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal10;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"parent";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal11;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"preceding";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal12;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"preceding-sibling";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Axis_Name_Nonterminal13;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"self";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Node_Test_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Name_Test_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Node_Test_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Node_Type_Part.all, Context_Node, Value);
      Append(Value.S, "()");
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Node_Test_Nonterminal3;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Literal_Value : Expression_Values(As_Expr_Text);
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.Processing_Instruction_Part.Token_String.all;
      Evaluate(This.Literal_Part.all, Context_Node, Literal_Value);
      Append(Value.S, "(" & Literal_Value.S & ")");
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Node_Type_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.Comment_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Node_Type_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.Text_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Node_Type_Nonterminal3;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.Processing_Instruction_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Node_Type_Nonterminal4;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.Node_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Name_Test_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"*";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Name_Test_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Qname_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Name_Test_Nonterminal3;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Ncname_Or_ID_Part.all, Context_Node, Value);
      Append(Value.S, ":*");
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     QName_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.NCName_Or_ID_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     QName_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Part2_Value : Expression_Values(As_Expr_Text);
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.NCName_Or_ID_Part1.all, Context_Node, Value);
      Evaluate(This.NCName_Or_ID_Part2.all, Context_Node, Part2_Value);
      Append(Value.S, ":" & Part2_Value.S);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     NCNAME_Or_ID_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.NCName_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     NCNAME_Or_ID_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := Get_Axis_Name(This.Axis_Name_Part.all);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     NCNAME_Or_ID_Nonterminal3;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := Get_Node_Type_Name(This.Node_Type_Part.all);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     NCNAME_Or_ID_Nonterminal4;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.And_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     NCNAME_Or_ID_Nonterminal5;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.Or_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     NCNAME_Or_ID_Nonterminal6;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.Mod_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     NCNAME_Or_ID_Nonterminal7;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.Div_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Predicate_Expr_Nonterminal;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is

   begin
      if Value.Value_Type = As_Expr_Text then
         Evaluate(This.Expr_Part.all, Context_Node, Value);
      else
         Evaluate(This.Expr_Part.all, Context_Node, Value);

         -- If this is numeric, then check to see if this node occupies
         --  the designated position
         if (Value.Value_Type = Expressions.As_Number)
           and then (Value.Special = Normal) then
            if Long_Float'Floor(Value.F) = Long_Float(Context_Node.Node_Position) then
               Value := (As_Boolean, True);
            else
               Value := (As_Boolean, False);
            end if;
         end if;
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate
     (This         : in     Abbreviated_Absolute_Location_Path_Nonterminal;
      Context_Node : in     Node_Items;
      Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Relative_Location_Path_Part.all, Context_Node, Value);
      Value.S := "//" & Value.S;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate
     (This         : in     Abbreviated_Relative_Location_Path_Nonterminal;
      Context_Node : in     Node_Items;
      Value        :    out Expression_Values)
   is
      Step_Part_Value : Expression_Values(As_Expr_Text);
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Evaluate(This.Relative_Location_Path_Part.all, Context_Node, Value);
      Evaluate(This.Step_Part.all, Context_Node, Step_Part_Value);
      Append(Value.S, "//" & Step_Part_Value.S);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

             procedure Evaluate (This         : in     Abbreviated_Step_Nonterminal1;
                                 Context_Node : in     Node_Items;
                                 Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +".";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Abbreviated_Step_Nonterminal2;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +"..";
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Expr_Nonterminal;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Or_Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Primary_Expr_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Put_Line("Evaluating Token: " &  "    Primary_Expr_Nonterminal1");
      Value := (As_Boolean, True);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Primary_Expr_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Primary_Expr_Nonterminal3;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Value := (Value_Type => As_String,
                S          => Get_Literal(This.Literal_Part));
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Primary_Expr_Nonterminal4;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Float_Value : Long_Float;
      Special     : Special_Number_Values;
   begin
      Get_Number(This.Number_Part, Float_Value, Special);
      Value := (Value_Type => As_Number,
                F          => Float_Value,
                Special    => Special);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Primary_Expr_Nonterminal5;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Function_Call_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Variable_Reference_Nonterminal;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Put_Line("Evaluating Token: " &  "    Variable_Reference_Nonterminal");
      Value := (As_Boolean, True);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Function_Call_Nonterminal;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Function_Name : String_Ptr;

      Args : Argument_List := Evaluate_Arguments(This.Arguments_Part, Context_Node);
   begin
      -- Just reach down and get the function name
      Function_Name :=
        NCName_Or_ID_Nonterminal1
        (Qname_Nonterminal1(This.Function_Name_Part.Qname_Part.all).NCName_Or_ID_Part.all).
        NCName_Part.Token_String;

      Evaluate_Function(Function_Name, Context_Node, Args, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Arguments_Nonterminal1;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      Put_Line("Evaluating Token: " &  "Arguments_Nonterminal1");
      Value := (As_Boolean, True);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Arguments_Nonterminal2;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      Put_Line("Evaluating Token: " &  "Arguments_Nonterminal2");
      Value := (As_Boolean, True);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Arguments_Nonterminal3;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Put_Line("Evaluating Token: " &  "Arguments_Nonterminal3");
      Value := (As_Boolean, True);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Argument_Nonterminal;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Function_Name_Nonterminal;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Put_Line("Evaluating Token: " &  "    Function_Name_Nonterminal");
      Value := (As_Boolean, True);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Union_Expr_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Path_Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Union_Expr_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Op1, Op2 : Expression_Values;

   begin
      Evaluate(This.Union_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Path_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & "|" & Op2.S;
      else
         -- Evaluate the union of the ops
         Coerce(Op1, As_Node_List);
         Coerce(Op2, As_Node_List);

         -- Still here?  Okay, merge the two sets.
         Value := Op1;
         for I in 0 .. Dom.Core.Nodes.Length(Op2.Ns) - 1 loop
            if not Is_In(Dom.Core.Nodes.Item(Op2.Ns, I), Value.Ns) then
               Dom.Core.Append_Node(Value.Ns, Dom.Core.Nodes.Item(Op2.Ns, I));
            end if;
         end loop;
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Path_Expr_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Location_Path_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Path_Expr_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Filter_Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Path_Expr_Nonterminal3;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Put_Line("Evaluating Token: " &  "    Path_Expr_Nonterminal3");
      Value := (As_Boolean, True);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Path_Expr_Nonterminal4;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Put_Line("Evaluating Token: " &  "    Path_Expr_Nonterminal4");
      Value := (As_Boolean, True);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Filter_Expr_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Primary_Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Filter_Expr_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Put_Line("Evaluating Token: " &  "    Filter_Expr_Nonterminal2");
      Value := (As_Boolean, True);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Or_Expr_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.And_Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Or_Expr_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Or_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.And_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & " or " & Op2.S;
      else
         Coerce(Op1, As_Boolean);
         Coerce(Op2, As_Boolean);
         Value := (Value_Type => As_Boolean,
                   B          => Op1.B or Op2.B);
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     And_Expr_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Equality_Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     And_Expr_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.And_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Equality_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & " and " & Op2.S;
      else
         Coerce(Op1, As_Boolean);
         Coerce(Op2, As_Boolean);
         Value := (Value_Type => As_Boolean,
                   B          => Op1.B and Op2.B);
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Equality_Expr_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Relational_Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Equality_Expr_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      -- EQ
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Equality_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Relational_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & "=" & Op2.S;
      else
         Value := (Value_Type => As_Boolean,
                   B          => Expressions.Compare(Op1, Op2, Equal));
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Equality_Expr_Nonterminal3;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      -- NE
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Equality_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Relational_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & "!=" & Op2.S;
      else
         Value := (Value_Type => As_Boolean,
                   B          => Expressions.Compare(Op1, Op2, Not_Equal));
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Relational_Expr_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Additive_Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Relational_Expr_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Relational_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Additive_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & "<" & Op2.S;
      else
         Value := (Value_Type => As_Boolean,
                   B          => Expressions.Compare(Op1, Op2, Less_Than));
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Relational_Expr_Nonterminal3;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Relational_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Additive_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & ">" & Op2.S;
      else
         Value := (Value_Type => As_Boolean,
                   B          => Expressions.Compare(Op1, Op2, Greater_Than));
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Relational_Expr_Nonterminal4;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Relational_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Additive_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & "<=" & Op2.S;
      else
         Value := (Value_Type => As_Boolean,
                   B          => Expressions.Compare(Op1, Op2, Less_Or_Equal));
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Relational_Expr_Nonterminal5;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Relational_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Additive_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & ">=" & Op2.S;
      else
         Value := (Value_Type => As_Boolean,
                   B          => Expressions.Compare(Op1, Op2, Greater_Or_Equal));
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Additive_Expr_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Multiplicative_Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Additive_Expr_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Additive_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Multiplicative_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & " + " & Op2.S;
      else
         Coerce(Op1, As_Number);
         Coerce(Op2, As_Number);
         Value := Expressions.Compute(Op1, Op2, Expressions.Add);
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Additive_Expr_Nonterminal3;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Additive_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Multiplicative_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & " - " & Op2.S;
      else
         Coerce(Op1, As_Number);
         Coerce(Op2, As_Number);
         Value := Expressions.Compute(Op1, Op2, Expressions.Subtract);
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Multiplicative_Expr_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Unary_Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Multiplicative_Expr_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Multiplicative_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Unary_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & " * " & Op2.S;
      else
         Coerce(Op1, As_Number);
         Coerce(Op2, As_Number);
         Value := Expressions.Compute(Op1, Op2, Expressions.Multiply);
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Multiplicative_Expr_Nonterminal3;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Multiplicative_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Unary_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & " div " & Op2.S;
      else
         Coerce(Op1, As_Number);
         Coerce(Op2, As_Number);
         Value := Expressions.Compute(Op1, Op2, Expressions.Divide);
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Multiplicative_Expr_Nonterminal4;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
      Op1, Op2 : Expression_Values;
   begin
      Evaluate(This.Multiplicative_Expr_Part.all, Context_Node, Op1);
      Evaluate(This.Unary_Expr_Part.all, Context_Node, Op2);

      if Value.Value_Type = As_Expr_Text then
         Value.S := Op1.S & " mod " & Op2.S;
      else
         Coerce(Op1, As_Number);
         Coerce(Op2, As_Number);
         Value := Expressions.Compute(Op1, Op2, Expressions.Modulo);
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Unary_Expr_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Union_Expr_Part.all, Context_Node, Value);
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Unary_Expr_Nonterminal2;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      Evaluate(This.Unary_Expr_Part.all, Context_Node, Value);
      if Value.Value_Type = As_Expr_Text then
         Value.S := "-" & Value.S;
      else
         Coerce(Value, As_Number);
         Value := Expressions.Compute(Value, Value, Expressions.Negate);
      end if;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This         : in     Number_Nonterminal1;
                       Context_Node : in     Node_Items;
                       Value        :    out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.Integer_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Number_Nonterminal2;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.Decimal_Literal_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Literal_Nonterminal1;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.DQ_Literal_Part.Token_String.all;
   end Evaluate;

   -------------
   -- Evaluate --
   -------------

   procedure Evaluate (This : in Literal_Nonterminal2;
                       Context_Node : in     Node_Items;
                      Value : out Expression_Values) is
   begin
      pragma Assert(Value.Value_Type = As_Expr_Text);
      Value.S := +This.SQ_Literal_Part.Token_String.all;
   end Evaluate;

   ----------------------------------------------------------

   function Evaluate_Arguments (Arguments_Part : Arguments_Nonterminal_Ptr;
                                Context_Node   : Node_Items)
                               return Argument_List is
      Value : Expression_Values;
      use type Ada.Tags.Tag;
   begin
      if Arguments_Part.all'Tag = Arguments_Nonterminal1'Tag then
         return No_Arguments;
      elsif Arguments_Part.all'Tag = Arguments_Nonterminal2'Tag then
         Evaluate(Arguments_Nonterminal2(Arguments_Part.all).Argument_Part.all,
                  Context_Node,
                  Value);
         return (1 => Value);
      else
         pragma Assert(Arguments_Part.all'Tag = Arguments_Nonterminal3'Tag);
         Evaluate(Arguments_Nonterminal3(Arguments_Part.all).Argument_Part.all,
                  Context_Node,
                  Value);
         return Evaluate_Arguments(Arguments_Nonterminal3(Arguments_Part.all).Arguments_Part,
                                   Context_Node) & Value;
      end if;
   end Evaluate_Arguments;

   ----------------------------------------------------------

end xia_parser_Model;

