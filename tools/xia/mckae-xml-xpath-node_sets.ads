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

with Ada.Containers.Vectors;

with Dom.Core;
use  Dom.Core;

with Mckae.XML.XPath.Locations;
use  Mckae.XML.XPath.Locations;

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

   function "=" (L, R : Current_Matchings) return Boolean;

   package Matchings_Sets
   is new Ada.Containers.Vectors (Index_Type => Natural,
                                  Element_Type => Current_Matchings);

   type Set is new Matchings_Sets.Vector with private;

   not overriding
   procedure Insert (Container : in out Set;
                     New_Item : Current_Matchings);
   --  Inserts New_Item (at the beginning) unless it's already present.

   overriding
   procedure Append (Container : in out Set;
                     New_Item : Current_Matchings;
                     Count : Ada.Containers.Count_Type := 1);
   --  Appends New_Item unless it's already present.
   --
   --  This subprogram can't sensibly be written as 'not overriding',
   --  leaving out the Count parameter. If we did, users would have no
   --  way of distinguishing a call to this subprogram from one to the
   --  inherited subprgram with Count defaulted.

   not overriding
   procedure Union (Target : in out Set; Source : Set);
   --  Union inserts into Target the elements of Source that are not
   --  equivalent to some element already in Target.

private
   type Set is new Matchings_Sets.Vector with null record;

end Mckae.XML.XPath.Node_Sets;
