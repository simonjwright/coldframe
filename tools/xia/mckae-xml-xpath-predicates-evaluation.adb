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

with Mckae.XML.XPath.Expressions;
with Xia_Parser_Model;

package body McKae.XML.XPath.Predicates.Evaluation is

   use Xia_Parser_Model;

   ----------------------------------------------------------------------

   procedure Evaluate_Predicate (Nodes            : in out Node_Sets.Set;
                                 Handle           : in     Predicate_Handles;
                                 Originating_Axis : in     Locations.Axes) is

      use Mckae.XML.XPath;

      Iter : Node_Sets.Matchings_Containers.Iterator'Class := Node_Sets.New_Iterator(Nodes);
      Node_Set_Size      : Natural := Node_Sets.Length(Nodes);
      Node_Item_Index    : Natural := 0;
      Node_Item_Position : Natural := 0;
      Node_Item          : Expressions.Node_Items;

      Expression : Expressions.Expression_Values;
      Predicate_Count : constant Natural :=
        Predicate_Handle_Pkg.Length(Handle.Predicate_List);

      Filtered_Nodes : Node_Sets.Set;

   begin
      if Predicate_Count >= 1 then
         for P in 1 .. Predicate_Count loop
            Node_Item_Index := 1;
            Node_Set_Size   := Node_Sets.Length(Nodes);
            Node_Sets.Matchings_Containers.Reset(Iter);

            while not Node_Sets.Matchings_Containers.Is_Done(Iter) loop
               if Locations.Forward_Axis(Originating_Axis) then
                  Node_Item_Position := Node_Item_Index;
               else
                  pragma Assert(Locations.Reverse_Axis(Originating_Axis));
                  Node_Item_Position := (Node_Set_Size - Node_Item_Index) + 1;
               end if;

               Node_Item := (N             => Node_Sets.Matchings_Containers.Current_Item(Iter).
                                                      Matching_Node,
                             Node_Position => Node_Item_Position,
                             Node_Set_Size => Node_Set_Size);
               Xia_Parser_Model.Evaluate(Predicate_Handle_Pkg.Item_At(Handle.Predicate_List, P).all,
                                         Node_Item,
                                         Expression);
               Expressions.Coerce(Expression, Expressions.As_Boolean);

               if Expression.B then
                  Node_Sets.Append(Filtered_Nodes, Node_Sets.Matchings_Containers.Current_Item(Iter));
               end if;

               Node_Sets.Matchings_Containers.Next(Iter);
               Node_Item_Index := Node_Item_Index + 1;
            end loop;
            Node_Sets.Clear(Nodes);
            Nodes := Filtered_Nodes;
            Node_Sets.Clear(Filtered_Nodes);
         end loop;
      end if;

   exception
      when Expressions.Invalid_Coercion
        | Expressions.Invalid_Expression =>
         raise Malformed_Predicate;
   end Evaluate_Predicate;

   ----------------------------------------------------------------------

end McKae.XML.XPath.Predicates.Evaluation;

