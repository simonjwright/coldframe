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

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Dom.Core.Append_Node;
--with Dom.Core.Attrs;
with Dom.Core.Nodes;

with Mckae.Xml.Xpath.Locations;
use  Mckae.Xml.Xpath.Locations;
with Mckae.Xml.Xpath.Node_Sets;
with Mckae.XML.XPath.Predicates;
with Mckae.XML.XPath.Predicates.Evaluation;
with McKae.XML.XPath.Query_Handling;

with XIA_Parser_Parser;

package body Mckae.XML.XPath.XIA is

   use Ada.Strings;
   use Dom.Core;
   use Dom.Core.Nodes;

   -------------------------------------------------------------------

   procedure Do_Final_Traversal (N             : in     Node;
                                 Location_Step : in     Location_Steps;
                                 Matchings     : in out Node_Sets.Set;
                                 Include_Self  : in     Boolean := True);

   -------------------------------------------------------------------

   procedure Get_Index_And_List (Of_Node : in     Node;
                                 Index   :    out Natural;
                                 List    :    out Node_List) is
      P : Node := Parent_Node(Of_Node);

   begin
      Index := 0;
      if P /= null then
         List := Child_Nodes(P);
         for I in 1 .. Length(List) loop
            Index := I - 1;
            exit when Of_Node = Item(List, Index);
         end loop;
      end if;
   end Get_Index_And_List;

   -------------------------------------------------------------------

   function Query_Separator_Index(S : String) return Natural is

      -- Search for a '|' in the string, which signifies the separator
      --  between queries being unioned together.  However, beware
      --  such vertical bars embedded in strings and predicates.

      End_Quote   : Character := '"';
      End_Predicate : constant Character := ']';
      Quoting     : Boolean := False;
      Predicating : Boolean := False;

   begin
      for I in S'Range loop
         if Quoting then
            -- Ignore everything other than the match close quote character
            Quoting := S(I) /= End_Quote;
         elsif Predicating then
            -- Ignore everything until the predicate's end bracket
            Predicating := S(I) /= End_Predicate;
            if S(I) = ''' then
               End_Quote := ''';
               Quoting := True;
            elsif S(I) = '"' then
               End_Quote := '"';
               Quoting := True;
            end if;
         else
            -- Could be anything, check to see if it's special.  If
            --  not signaling the start of a quote or predicate, just
            --  keep on going.
            if S(I) = ''' then
               End_Quote := ''';
               Quoting := True;
            elsif S(I) = '"' then
               End_Quote := '"';
               Quoting := True;
            elsif S(I) = '[' then
               Predicating := True;
            elsif S(I) = '|' then
               return I;
            end if;
         end if;
      end loop;
      return 0;
   end Query_Separator_Index;

   -------------------------------------------------------------------

   procedure Predicate_Filtration (Matchings     : in out Node_Sets.Set;
                                   Location_Step : in     Location_Steps) is
   begin
      Predicates.Evaluation.Evaluate_Predicate
        (Matchings, Location_Step.Location_Predicates, Location_Step.Axis);
   exception
      when Predicates.Malformed_Predicate =>
         raise Malformed_Xpath;
   end Predicate_Filtration;

   -------------------------------------------------------------------

   function Is_Node_Match (N  : Node;
                           Ls : Location_Steps)
                          return Boolean is

      use type Predicates.Predicate_Handles;

      NT    : Node_Test_Specification := Ls.Node_Test;
      Match : Boolean := False;

   begin
      case NT.Node_Test is
         when Node_Node_Test =>
            -- Matches anything
            Match := True;

         when NCName_Node_Test =>
            Match := (((N.Node_Type = Element_Node) or (N.Node_Type = Attribute_Node))
                       and (NT.Name = "*"))
                      or (NT.Name = Node_Name(N));

         when QName_Node_Test =>
            Match := ((NT.Prefix = "*") and ((NT.Name = "*") or (NT.Name = Prefix(N))))
              or
              ((NT.Name = "*") and ((NT.Prefix = "*") or (NT.Prefix = Local_Name(N))))
              or
              ((NT.Prefix = Prefix(N)) and (NT.Name = Local_Name(N)));

         when Text_Node_Test =>
            Match := N.Node_Type = Text_Node;

         when Comment_Node_Test =>
            Match := N.Node_Type = Comment_Node;

         when Processing_Instruction_Node_Test =>
            Match := (N.Node_Type = Processing_Instruction_Node)
              and NT.Name = Node_Value(N);

         when No_Node_Test =>
            pragma Assert (False);
            null;

      end case;

      return Match;
   end Is_Node_Match;

   -------------------------------------------------------------------

   procedure Do_Matching (N             : in     Node;
                          Location_Step : in     Location_Steps;
                          Matchings     : in out Node_Sets.Set;
                          Attr_Parent   : in     Node := null) is

      Ancestor : Node;
      Child    : Node;
      Children : Node_List;
      N_Index  : Natural;
      Attr     : Node;
      Attrs    : Named_Node_Map;

      use Node_Sets;

      -- This function masks the DOM.Core.Parent_Node function so as
      --  to work around its bug when it comes to returning the parent
      --  of attribute nodes
      function Parent_Node(N : in Node) return Node is
      begin
         if Attr_Parent = null then
            -- No attribute parent node was provided, so the node was
            --  not an attribute
            return DOM.Core.Nodes.Parent_Node(N);
         else
            return Attr_Parent;
         end if;
      end Parent_Node;

   begin

      -- The first set of axes is for those for which an immediate
      --  determination of node matching is made.
      case Location_Step.Axis is
         when Child_Axis =>
            -- Check to see if any of the children match
            Children := Child_Nodes(N);
            for I in 1 .. Length(Children) loop
               Child := Item(Children, I - 1);
               if Is_Node_Match(Child, Location_Step) then
                  Matchings.Append ((Self_Axis, Child));
               end if;
            end loop;

         when Following_Sibling_Axis =>
            Get_Index_And_List(N, N_Index, Children);
            for I in N_Index + 1 .. Length(Children) - 1 loop
               Child := Item(Children, I);
               if Is_Node_Match(Child, Location_Step) then
                  Matchings.Append ((Self_Axis, Child));
               end if;
            end loop;

         when Preceding_Sibling_Axis =>
            Get_Index_And_List(N, N_Index, Children);
            for I in 0 .. N_Index - 1 loop
               Child := Item(Children, I);
               if Is_Node_Match(Child, Location_Step) then
                  Matchings.Append ((Self_Axis, Child));
               end if;
            end loop;

         when Self_Axis | Parent_Axis =>
            -- Get the starting node for the upwards traversal
            if Location_Step.Axis = Parent_Axis then
               Ancestor := Parent_Node(N);

            else
               Ancestor := N;
            end if;

            -- Check the node for a match
            if (Ancestor /= null)
              and then Is_Node_Match(Ancestor, Location_Step) then
               Matchings.Append ((Self_Axis, Ancestor));
            end if;

         when Ancestor_Axis | Ancestor_Or_Self_Axis =>
            -- Get the starting node for the upwards traversal
            if Location_Step.Axis = Ancestor_Axis then
               Ancestor := Parent_Node(N);
            else
               Ancestor := N;  -- I am my mother :-)
            end if;

            -- Any matches in the ancestor chain?
            loop
               exit when Ancestor = null;
               if Is_Node_Match(Ancestor, Location_Step) then
                  Matchings.Insert ((Self_Axis, Ancestor));
               end if;
               Ancestor := Parent_Node(Ancestor);
            end loop;

         when Attribute_Axis =>
            Attrs := Attributes(N);
            for I in 1 .. Length(Attrs) loop
               Attr := Item(Attrs, I - 1);
               if Is_Node_Match(Attr, Location_Step) then
                  Matchings.Append ((Attribute_Axis,
                                     Attr,
                                     Owner_Node => N,
                                     Attr_Index => I));
               end if;
            end loop;

            -- The tests for the remaining axes is deferred
            --  until they can be done in conjunction with the
            --  next path step

         when Branched_Axes =>
            if not Location_Step.Output_Step then
               declare
                  Deferred_Match : Node_Sets.Current_Matchings(Location_Step.Axis);
               begin
                  Deferred_Match.Matching_Node := N;
                  Deferred_Match.Branch_Step := Location_Step;
                  Matchings.Append (Deferred_Match);
               end;
            else
               Do_Final_Traversal(N, Location_Step, Matchings,
                                  Location_Step.Axis = Descendant_Or_Self_Axis);
            end if;

         when Namespace_Axis =>
            pragma Assert(False);
            null;  -- TBD
      end case;
   end Do_Matching;

   -------------------------------------------------------------------

   procedure Descendant_Traversal (N             : in     Node;
                                   Deferred_Step : in     Location_Steps;
                                   Location_Step : in     Location_Steps;
                                   Matchings     : in out Node_Sets.Set;
                                   Include_Self  : in     Boolean           := True) is
      Children : Node_List;
      Child    : Node;

   begin
      -- This node matches the original traversal-requiring match, now
      --  check the next location step
      if Include_Self and Is_Node_Match(N, Deferred_Step) then
         Do_Matching(N, Location_Step, Matchings);
      end if;

      -- Process the child nodes
      Children := Child_Nodes(N);
      for I in 1 .. Length(Children) loop
         Child := Item(Children, I - 1);
         Descendant_Traversal(Child, Deferred_Step, Location_Step, Matchings);
      end loop;
   end Descendant_Traversal;

   -------------------------------------------------------------------

   procedure Following_Traversal (N             : in     Node;
                                  Deferred_Step : in     Location_Steps;
                                  Location_Step : in     Location_Steps;
                                  Matchings     : in out Node_Sets.Set) is

      -- Process the rest of the nodes in this list and their
      --  descendants, then go to the parent and process all its
      --  subsequent nodes

      This_List : Node_List;
      N_Index   : Natural;

   begin
      if N /= null then
         Get_Index_And_List(N, N_Index, This_List);

         -- Process the following siblings and all descendants
         for I in N_Index + 1 .. Length(This_List) - 1 loop
            Descendant_Traversal(Item(This_List, I), Deferred_Step, Location_Step, Matchings);
         end loop;

         -- Now go to the parent
         Following_Traversal(Parent_Node(N), Deferred_Step, Location_Step, Matchings);
      end if;
   end Following_Traversal;

   -------------------------------------------------------------------

   procedure Preceding_Traversal (N             : in     Node;
                                  Deferred_Step : in     Location_Steps;
                                  Location_Step : in     Location_Steps;
                                  Matchings     : in out Node_Sets.Set) is
      -- Process the previous of the nodes in this list and their
      --  descendants, then go to the parent and process all its
      --  preceding nodes

      This_List : Node_List;
      N_Index   : Natural;

   begin
      if N /= null then
         Get_Index_And_List(N, N_Index, This_List);

         -- Process the following siblings and all descendants
         for I in 0 .. N_Index - 1 loop
            Descendant_Traversal(Item(This_List, I), Deferred_Step, Location_Step, Matchings);
         end loop;

         -- Now go to the parent
         Preceding_Traversal(Parent_Node(N), Deferred_Step, Location_Step, Matchings);
      end if;
   end Preceding_Traversal;

   -------------------------------------------------------------------

   procedure Deferred_Traversal (Location_Step : in     Location_Steps;
                                 Match         : in     Node_Sets.Current_Matchings;
                                 Matchings     : in out Node_Sets.Set) is
   begin
      case Match.Branch_Step.Axis is
         when Descendant_Or_Self_Axis |
           Descendant_Axis =>
            Descendant_Traversal(Match.Matching_Node,
                                 Match.Branch_Step,
                                 Location_Step,
                                 Matchings,
                                 Match.Branch_Step.Axis = Descendant_Or_Self_Axis);

         when Following_Axis =>
            Following_Traversal(Match.Matching_Node,
                                Match.Branch_Step,
                                Location_Step,
                                Matchings);

         when Preceding_Axis =>
            Preceding_Traversal(Match.Matching_Node,
                                Match.Branch_Step,
                                Location_Step,
                                Matchings);

         when others =>
            pragma Assert(False);
            null;
      end case;
   end Deferred_Traversal;

   -------------------------------------------------------------------

   procedure Do_Final_Traversal (N             : in     Node;
                                 Location_Step : in     Location_Steps;
                                 Matchings     : in out Node_Sets.Set;
                                 Include_Self  : in     Boolean := True) is
      Children : Node_List;
      Child    : Node;
      Siblings : Node_List;
      N_Index  : Natural;

      Mod_Step : Location_Steps := Location_Step;

   begin
      case Location_Step.Axis is
         when Descendant_Or_Self_Axis |
           Descendant_Axis =>
            -- This node matches the original traversal-requiring match, now
            --  check the next location step
            if Include_Self and Is_Node_Match(N, Location_Step) then
               Matchings.Append ((Self_Axis, N));
            end if;

            -- Process the child nodes
            Children := Child_Nodes(N);
            for I in 1 .. Length(Children) loop
               Child := Item(Children, I - 1);
               Do_Final_Traversal(Child, Location_Step, Matchings);
            end loop;

         when Following_Axis =>
            if N /= null then
               Get_Index_And_List(N, N_Index, Siblings);
               Mod_Step.Axis := Descendant_Axis;

               -- Process the following siblings and all descendants
               for I in N_Index + 1 .. Length(Siblings) - 1 loop
                  Child := Item(Siblings, I);
                  Do_Final_Traversal(Child, Mod_Step, Matchings);
               end loop;

               Do_Final_Traversal(Parent_Node(N), Location_Step, Matchings, Include_Self => False);
            end if;

         when Preceding_Axis =>
            if N /= null then
               Get_Index_And_List(N, N_Index, Siblings);
               Mod_Step.Axis := Descendant_Axis;

               -- Process the preceding siblings and all descendants
               for I in 0 .. N_Index - 1 loop
                  Child := Item(Siblings, I);
                  Do_Final_Traversal(Child, Mod_Step, Matchings);
               end loop;

               Do_Final_Traversal(Parent_Node(N), Location_Step, Matchings, Include_Self => False);
            end if;

         when others =>
            pragma Assert(False);
            null;
      end case;
   end Do_Final_Traversal;

   -------------------------------------------------------------------

   procedure Extract_Nodes (Location_Step : in     Location_Steps;
                            Matchings     : in out Node_Sets.Set) is

      Match         : Node_Sets.Current_Matchings;
      New_Matchings : Node_Sets.Set;
      Cursor        : Node_Sets.Cursor := Matchings.First;

      use type Node_Sets.Cursor;
      use type Predicates.Predicate_Handles;

   begin
      -- Iterate through the current set of matchings, evaluating each
      --  against the path step
      while Cursor /= Node_Sets.No_Element loop
         Match := Node_Sets.Element (Cursor);

         -- Check whether this is a direct node check, or a deferred traversal
         if Match.Axis = Self_Axis then
            Do_Matching(Match.Matching_Node, Location_Step, New_Matchings);

         elsif Match.Axis = Attribute_Axis then
            -- This is a workaround for an XML/Ada bug wherein
            --  invoking Parent_Node() on an attribute node returns
            --  null instead of its parent.
            Do_Matching(Match.Matching_Node,
                        Location_Step,
                        New_Matchings,
                        Match.Owner_Node);

         else
            -- A deferred traversal
            Deferred_Traversal(Location_Step, Match, New_Matchings);
         end if;

         Node_Sets.Next (Cursor);
      end loop;

      -- Now check the predicate criteria
      if Location_Step.Location_Predicates /= Predicates.Null_Predicate then
         Predicate_Filtration(New_Matchings, Location_Step);
      end if;

      Matchings := New_Matchings;

   exception
      when Xia_Parser_Parser.Syntax_Error =>
        raise Malformed_XPath;
   end Extract_Nodes;

   -------------------------------------------------------------------

   procedure Evaluate_Location_Path
     (XPath     : in     String;
      Matchings : in out Node_Sets.Set)
   is
      Query : String := Trim(XPath, Both);

      Location_Steps : Location_Paths;

   begin
      -- Split up the location path into discrete steps
      Location_Steps := Query_Handling.Pathify(Query);

      for S in 1 .. Location_Steps.Steps loop
         Extract_Nodes(Location_Steps.Path(S), Matchings);

         exit when Matchings.Is_Empty;
      end loop;

      Free(Location_Steps);

   exception
      when Query_Handling.Malformed_Query =>
         raise Malformed_XPath;
   end Evaluate_Location_Path;

   -------------------------------------------------------------------

   --  This is used to obtain the set of matching nodes in document
   --  order.
   package Sortable_Matching_Tree
   is new Ada.Containers.Ordered_Maps (Key_Type => Unbounded_String,
                                       Element_Type => DOM.Core.Node);

   -------------------------------------------------------------------

   function Create_Node_Key (For_Node : Node) return Unbounded_String
   is

      This_List  : Node_List;
      Index      : Natural;
      Parent     : Node;

      Result : Unbounded_String;

   begin
      Get_Index_And_List(For_Node, Index, This_List);

      Parent := Parent_Node (For_Node);
      if Parent /= null then
         Result := Create_Node_Key (For_Node => Parent);
      end if;

      declare
         Zeroed_Index : constant Natural := Index + 1000000;
         Index_Img : constant String := Natural'Image(Zeroed_Index);
      begin
         Append (Result, Index_Img(3 .. Index_Img'Last));
      end;
      return Result;
   end Create_Node_Key;

   -------------------------------------------------------------------

   procedure Output_Nodes (M     : in     Sortable_Matching_Tree.Map;
                           Nodes :    out Node_List)
   is
      procedure Output_Node (Position : in Sortable_Matching_Tree.Cursor)
      is
      begin
         Dom.Core.Append_Node (Nodes,
                               Sortable_Matching_Tree.Element (Position));
      end Output_Node;
   begin
      M.Iterate (Process => Output_Node'Access);
   end Output_Nodes;

   -------------------------------------------------------------------

   procedure Finalize_Matchings (Matchings   : in     Node_Sets.Set;
                                 Xpath_Nodes :    out Node_List) is

      Sorting_Tree   : Sortable_Matching_Tree.Map;

      Cursor        : Node_Sets.Cursor := Matchings.First;

      Current       : Node_Sets.Current_Matchings;
      Node_Key      : Unbounded_String;

      Sorted_Results  : Node_List;

      use type Node_Sets.Cursor;
   begin
      -- Perform a tree insertion sort of all the nodes
      while Cursor /= Node_Sets.No_Element loop

         -- Get the matching info for the node
         Current := Node_Sets.Element (Cursor);

         pragma Assert((Current.Axis = Self_Axis)
                       or (Current.Axis = Attribute_Axis));

         if Current.Axis = Attribute_Axis then
            --  Most nodes are simply a node on the DOM tree;
            --  attribute nodes are "special" for some reason, so we
            --  have to go get their owner element specifically.
            --
            --  XXX this may still be an XML/Ada bug; in XML/Ada 1.0,
            --  Attrs.Owner_Element(Current.Matching_Node) didn't
            --  work.
            --
            --  Make the key for the parent ...
            Node_Key := Create_Node_Key (Current.Owner_Node);
            --  ... and append the key for the attribute.
            declare
               Zeroed_Index : constant Natural := Current.Attr_Index + 1000000;
               Index_Img : constant String := Natural'Image(Zeroed_Index);
            begin
               Append (Node_Key, Index_Img(3 .. Index_Img'Last));
            end;
         else
            Node_Key := Create_Node_Key (Current.Matching_Node);
         end if;

         Sorting_Tree.Insert (Node_Key, Current.Matching_Node);

         Node_Sets.Next (Cursor);
      end loop;

      -- All the nodes are now inserted, do an in-order traversal to
      --  get the nodes back in document order, and output them.
      Output_Nodes(Sorting_Tree, XPath_Nodes);
   end Finalize_Matchings;

   -------------------------------------------------------------------

   function XPath_Query
     (N     : Node;
      XPath : String) return Node_List
   is
      Starting_Node : Node := N;
      Xpath_Nodes   : Node_List;

      Start : Natural := 1;
      Split : Natural := 1;
      Query : String := Trim(XPath, Both);

      Matchings : Node_Sets.Set;
      Total_Matchings : Node_Sets.Set;

   begin
      loop
         Matchings.Clear;

         -- The given node is either a document node or an element node
         --  within a document.
         Starting_Node := N;

         if Query(Start) = '/' then
            Starting_Node := Owner_Document(N);
         end if;

         if Starting_Node = null then
            raise Inappropriate_Node;
         end if;

         Matchings.Append((Self_Axis, Starting_Node));

         -- Process this as a concatenation of queries, i.e.,
         --  different queries separated by '|'.  Watch out for '|'s
         --  embedded in predicates!
         Split := Query_Separator_Index(Query(Start .. Query'Last));

         if Split = 0 then
            Evaluate_Location_Path(Query(Start .. Query'Last), Matchings);
         else
            Evaluate_Location_Path(Query(Start .. Split - 1), Matchings);
         end if;

         -- Merge the distinct sets
         Total_Matchings.Union (Matchings);

         exit when Split = 0;

         Start := Split + 1;
      end loop;

      -- Iterate through the set of matchings, sorting them into
      --  document order and returning them as a Node_List;
      Finalize_Matchings(Total_Matchings, Xpath_Nodes);

      return Xpath_Nodes;
   end XPath_Query;

   -------------------------------------------------------------------

end Mckae.XML.XPath.XIA;
