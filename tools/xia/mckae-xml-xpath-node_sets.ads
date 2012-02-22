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

with Bc.Containers.Collections.Unbounded;
with Bc.Containers.Sets.Unbounded;
with Bc.Containers.Trees.Avl;
with Bc.Support.Standard_Storage;

with Dom.Core;
use  Dom.Core;

with Mckae.XML.XPath.Locations;
use  Mckae.XML.XPath.Locations;
with Mckae.Space.Unbounded_String_Expandable;
use  Mckae.Space.Unbounded_String_Expandable;

package Mckae.XML.XPath.Node_Sets is


   -- Record information about node-set member nodes
   type Current_Matchings (Axis : Axes := No_Axis) is record
      Matching_Node : Node;

      case Axis is
         when Attribute_Axis =>
            Owner_Node  : Node;    -- Work around bug in XMLAda wherein the
                                   -- parent node of an attribute is always
                                   -- set to Null, making it impossible to
                                   -- get the owner element
            Attr_Index  : Natural;

         when Branched_Axes =>
            Branch_Step : Location_Steps;

         when others =>
            null;
      end case;
   end record;

   -------------------------------------------------------------------

   package Matchings_Containers is new BC.Containers (Item => Current_Matchings);
   package Matchings_Collections is new Matchings_Containers.Collections;
   package Matchings_Sets is new Matchings_Collections.Unbounded
     (Storage => BC.Support.Standard_Storage.Pool);

   type Set is new Matchings_Sets.Collection with private;

   function Null_Container return Set;

   procedure Insert (C : in out Set; Elem : Current_Matchings);
   --  Add the item to the front of the collection, if it is not
   --  already in the set

   procedure Append (C : in out Set; Elem : Current_Matchings);
   --  Add the item to the collection, starting at the end, if it is
   --  not already in the set

   procedure Clear (C : in out Set);
   --  Empty the collection of all items.

   procedure Union (S : in out Set; O : Set);
   --  Perform a logical set union; at the completion of this
   --  operation, the set S contains the items found in its original
   --  state combined with the set O (but without duplication). For
   --  each item in the set O, if the item is not already a distinct
   --  member of the set S, copy the item and add it to the set S. If
   --  the item already is a member, do nothing.

   -------------------------------------------------------------------

   type Sortable_Matches is record
      Key          : Expandable_String;
      Matched_Node : Node;
   end record;

   function "<"(L, R : Sortable_Matches) return Boolean;

   package Sorting_Containers is new BC.Containers (Item => Sortable_Matches);

   package Match_Trees is new Sorting_Containers.Trees;

   package Sortable_Matching_Tree is new Match_Trees.Avl
     (Storage => BC.Support.Standard_Storage.Pool);

private
   -- Use some of the lower bits of the address of the "Node" pointer
   --  to be the hash value.
   function Generate_Hash(N : Node) return Natural;

   ---------------------------------------------------------------------

   package Match_Nodes_Containers is new BC.Containers (Item => Dom.Core.Node);
   package Match_Nodes_Sets       is new Match_Nodes_Containers.Sets;
   package Match_Nodes is new Match_Nodes_Sets.Unbounded
     (Hash => Generate_Hash,
      Buckets => 991,
      Storage => BC.Support.Standard_Storage.Pool);

   type Set is new Matchings_Sets.Collection with
      record
         Presence : Match_Nodes.Set;
      end record;

end Mckae.XML.XPath.Node_Sets;
