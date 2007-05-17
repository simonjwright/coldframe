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

--  This unit is used in place of the standard ColdFrame unbounded
--  storage pool.
--
--  The changes are,
--
--  * A limit is supplied such that if the allocated memory exceeds
--  the limit a log output will be generated whowing the allocation
--  sites. This is reported only on the first occurrence (to the file
--  "debug_storage_pool.log").
--
--  * A Report operation is provided to report on currently allocated
--  memory at any time.

--  $RCSfile: coldframe-unbounded_debug_storage_pools.ads,v $
--  $Revision: f80c44ac3b2a $
--  $Date: 2007/05/17 21:40:30 $
--  $Author: simonjwright $

with System.Storage_Elements;
with System.Storage_Pools;

with BC.Containers.Trees.AVL;
with BC.Support.Unmanaged_Storage;
with ColdFrame.Unbounded_Storage_Pools;
with GNAT.Traceback;

package ColdFrame.Unbounded_Debug_Storage_Pools is

   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators.

   type Unbounded_Pool (Limit : System.Storage_Elements.Storage_Count)
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

   procedure Report
     (Pool          : Unbounded_Pool;
      To_File_Named : String);

private

   protected type Mutex is
      entry Seize;
      procedure Release;
   private
      Seized : Boolean := False;
   end Mutex;

   subtype Tracebacks_Array is GNAT.Traceback.Tracebacks_Array (1 .. 10);

   type Allocation is record
      Storage_Address : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count;
      Call_Chain : Tracebacks_Array;
      Call_Chain_Length : Natural;
   end record;

   function "=" (L, R : Allocation) return Boolean;
   function "<" (L, R : Allocation) return Boolean;

   Tree_Pool : BC.Support.Unmanaged_Storage.Pool;
   --  GNAT 3.16a1 workround
   Pool : System.Storage_Pools.Root_Storage_Pool'Class
     renames System.Storage_Pools.Root_Storage_Pool'Class (Tree_Pool);

   package Abstract_Containers is new BC.Containers (Allocation);
   package Abstract_Trees is new Abstract_Containers.Trees;
   package Trees is new Abstract_Trees.AVL (Storage => Pool);

   type Unbounded_Pool (Limit : System.Storage_Elements.Storage_Count)
   is new ColdFrame.Unbounded_Storage_Pools.Unbounded_Pool with record
      Excluder : Mutex;
      Current_Allocation : System.Storage_Elements.Storage_Count := 0;
      Current_Allocations : Trees.AVL_Tree;
      Limit_Reached : Boolean := False;
   end record;

end ColdFrame.Unbounded_Debug_Storage_Pools;
