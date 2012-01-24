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

with Bc.Support.Standard_Storage;

with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body Mckae.XML.XPath.Node_Sets is


   ---------------------------------------------------------------------

   function Null_Container return Set is
      Empty: Set;
   begin
      return Empty;
   end Null_Container;

   ---------------------------------------------------------------------

   procedure Insert (C : in out Set; Elem : Current_Matchings) is
      --  Add the item to the front of the collection.
      Not_Present : Boolean := False;
      Item    : Current_Matchings := Elem;
   begin
      Match_Nodes.Add(C.Presence, Elem.Matching_Node, Not_Present);
      if Not_Present then
         Matchings_Sets.Insert(Matchings_Sets.Collection(C), Item);
      end if;
   end Insert;

   ---------------------------------------------------------------------

   procedure Append (C : in out Set; Elem : Current_Matchings) is
      --  Add the item to the collection, starting at the end, if it
      --  is not already in the set

      Not_Present : Boolean := False;
      Item    : Current_Matchings := Elem;
   begin
      Match_Nodes.Add(C.Presence, Elem.Matching_Node, Not_Present);
      if Not_Present then
         Matchings_Sets.Append(Matchings_Sets.Collection(C), Item);
      end if;
   end Append;

   ---------------------------------------------------------------------

   procedure Clear (C : in out Set)
   is
      -- Clear the node set _and_ the associated node presence list
   begin
      Matchings_Sets.Clear(Matchings_Sets.Collection(C));
      Match_Nodes.Clear(C.Presence);
   end Clear;

   -------------------------------------------------------------------

   procedure Union (S : in out Set; O : Set) is
      Iter : Matchings_Containers.Iterator'Class := New_Iterator(O);
   begin
      while not Matchings_Containers.Is_Done(Iter) loop
         Append(S, Matchings_Containers.Current_Item(Iter));
         Matchings_Containers.Next(Iter);
      end loop;
   end Union;

   ---------------------------------------------------------------------

   function "<"(L, R : Sortable_Matches) return Boolean is
   begin
     return L.Key < R.Key;
   end "<";

   ---------------------------------------------------------------------

   package Node_Hashing_Package is new System.Address_To_Access_Conversions(Dom.Core.Node_Record);

   -- Since Node is a pointer, and addresses are nearly always even,
   --  and also usually divisible by 4, drop the lower two bits of the
   --  address.
   function Generate_Hash(N : Node) return Natural is

      use System.Storage_Elements;
      use Node_Hashing_Package;

      -- Convert the node access value to an address
      Node_Addr : constant System.Address
        := To_Address(Node_Hashing_Package.Object_Pointer(N));

      Natural_Bit_Factor : constant
        System.Storage_Elements.Storage_Offset := (2 ** (Natural'Size - 1));
   begin
      return Natural(Node_Addr mod Natural_Bit_Factor) / 4;
   end Generate_Hash;

   -------------------------------------------------------------------

end Mckae.XML.XPath.Node_Sets;
