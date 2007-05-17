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

--  This unit is used in place of the standard storage pool.
--
--  The change is,
--
--  * allocations are initialized to an improbable value (16#deadbeef#)
--  * deallocations are filled to an improbable value (16#deaddead#)

--  $RCSfile: coldframe-unbounded_storage_pools.ads,v $
--  $Revision: 773e6667d19c $
--  $Date: 2007/05/17 21:41:45 $
--  $Author: simonjwright $

with System.Storage_Elements;
with System.Storage_Pools;

package ColdFrame.Unbounded_Storage_Pools is

   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators.

   type Unbounded_Pool
      is new System.Storage_Pools.Root_Storage_Pool with private;

   procedure Allocate
     (Pool                     : in out Unbounded_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment                : in System.Storage_Elements.Storage_Count);

   procedure Deallocate
     (Pool                     : in out Unbounded_Pool;
      Storage_Address          : in System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment                : in System.Storage_Elements.Storage_Count);

   function Storage_Size
     (Pool : Unbounded_Pool)
     return System.Storage_Elements.Storage_Count;

private

   type Unbounded_Pool
      is new System.Storage_Pools.Root_Storage_Pool with null record;

end ColdFrame.Unbounded_Storage_Pools;
