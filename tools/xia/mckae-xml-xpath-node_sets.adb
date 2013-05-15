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

package body McKae.XML.XPath.Node_Sets is


   function "=" (L, R : Current_Matchings) return Boolean is
   begin
      return L.Matching_Node = R.Matching_Node;
   end "=";

   -------------------------------------------------------------------

   not overriding
   procedure Insert (Container : in out Set;
                     New_Item : Current_Matchings)
   is
   begin
      if not Container.Contains (New_Item) then
         Container.Insert (New_Item => New_Item,
                           Before => Container.First_Index);
      end if;
   end Insert;

   -------------------------------------------------------------------
   overriding
   procedure Append (Container : in out Set;
                     New_Item : Current_Matchings;
                     Count : Ada.Containers.Count_Type := 1)
   is
   begin
      if not Container.Contains (New_Item) then
         Matchings_Sets.Vector (Container).Append (New_Item);
      end if;
   end Append;

   -------------------------------------------------------------------

   not overriding
   procedure Union (Target : in out Set; Source : Set) is
      procedure Append (Position : Matchings_Sets.Cursor) is
      begin
         Target.Append (New_Item => Matchings_Sets.Element (Position));
      end Append;
   begin
      Source.Iterate (Append'Access);
   end Union;

end Mckae.XML.XPath.Node_Sets;
