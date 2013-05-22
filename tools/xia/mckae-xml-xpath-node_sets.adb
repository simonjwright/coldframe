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

with System;

package body McKae.XML.XPath.Node_Sets is

   not overriding
   function Length (Container : Set) return Ada.Containers.Count_Type
   is
   begin
      return Container.Elements.Length;
   end Length;

   -------------------------------------------------------------------

   not overriding
   function Is_Empty (Container : Set) return Boolean
   is
   begin
      return Container.Elements.Is_Empty;
   end Is_Empty;

   -------------------------------------------------------------------

   not overriding
   procedure Clear (Container : in out Set)
   is
   begin
      Container.Elements.Clear;
      Container.Presence.Clear;
   end Clear;

   -------------------------------------------------------------------

   not overriding
   function First (Container : Set) return Cursor
   is
   begin
      return Cursor'(Elements_Cursor => Container.Elements.First);
   end First;

   -------------------------------------------------------------------

   function Element (Position : Cursor) return Current_Matchings
   is
   begin
      return Matchings_Vectors.Element (Position.Elements_Cursor);
   end Element;

   -------------------------------------------------------------------

   procedure Next (Position : in out Cursor)
   is
   begin
      Matchings_Vectors.Next (Position.Elements_Cursor);
   end Next;

   -------------------------------------------------------------------

   not overriding
   procedure Insert (Container : in out Set;
                     New_Item : Current_Matchings)
   is
   begin
      if not Container.Presence.Contains (New_Item) then
         Container.Presence.Insert (New_Item);
         Container.Elements.Insert (New_Item => New_Item,
                                    Before => Container.Elements.First_Index);
      end if;
   end Insert;

   -------------------------------------------------------------------

   not overriding
   procedure Append (Container : in out Set;
                     New_Item : Current_Matchings)
   is
   begin
      if not Container.Presence.Contains (New_Item) then
         Container.Presence.Insert (New_Item);
         Container.Elements.Append (New_Item);
      end if;
   end Append;

   -------------------------------------------------------------------

   not overriding
   procedure Union (Target : in out Set; Source : Set) is
      procedure Append (Position : Matchings_Vectors.Cursor) is
      begin
         Target.Append (New_Item => Matchings_Vectors.Element (Position));
      end Append;
   begin
      Source.Elements.Iterate (Append'Access);
   end Union;

   -------------------------------------------------------------------

   function "=" (L, R : Current_Matchings) return Boolean is
   begin
      return L.Matching_Node = R.Matching_Node;
   end "=";

   -------------------------------------------------------------------

   function "<" (L, R : Current_Matchings) return Boolean is
      use System;
   begin
      return L.Matching_Node.all'Address < R.Matching_Node.all'Address;
   end "<";

end Mckae.XML.XPath.Node_Sets;
