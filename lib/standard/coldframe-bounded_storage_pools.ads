--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  This unit is used in place of the access type Storage_Size
--  attribute, and uses the GNAT pool used in those circumstances
--  (System.Pool_Size.Stack_Bounded_Pool).
--
--  The changes are,
--
--  * allocations are initialized to an improbable value (16#deadbeef#)

pragma Warnings (Off);
with System.Pool_Size;
pragma Warnings (On);
with System.Storage_Elements;
with System.Storage_Pools;

package ColdFrame.Bounded_Storage_Pools is

   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   type Bounded_Pool
     (Pool_Size : System.Storage_Elements.Storage_Count;
      Elmt_Size : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count)
   is new System.Storage_Pools.Root_Storage_Pool with private;

   procedure Allocate
     (Pool         : in out Bounded_Pool;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   procedure Deallocate
     (Pool         : in out Bounded_Pool;
      Address      : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   function Storage_Size
     (Pool : Bounded_Pool)
     return System.Storage_Elements.Storage_Count;

private

   type Bounded_Pool
     (Pool_Size : System.Storage_Elements.Storage_Count;
      Elmt_Size : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count)
   is new System.Pool_Size.Stack_Bounded_Pool (Pool_Size => Pool_Size,
                                               Elmt_Size => Elmt_Size,
                                               Alignment => Alignment)
   with null record;

end ColdFrame.Bounded_Storage_Pools;
