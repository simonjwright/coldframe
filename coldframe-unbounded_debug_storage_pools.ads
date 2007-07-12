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
--  $Revision: 7a9dfe80e94f $
--  $Date: 2007/07/12 21:12:27 $
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

   --  Mutual exclusion while the pool structures are updated.

   protected type Mutex is
      entry Seize;
      procedure Release;
   private
      Seized : Boolean := False;
   end Mutex;

   --  Call site information. Call sites are identified by (the lowest
   --  10 entries) in the call chain leading to the call to
   --  Allocate. The stored information relates to allocations that
   --  have been freed and to current allocations (that have not been
   --  freed).

   Max_Stored_Call_Chain_Length : constant := 10;

   subtype Call_Chain_Count
      is Natural range 0 .. Max_Stored_Call_Chain_Length;

   type Call_Site_Data (Call_Chain_Length : Call_Chain_Count := 0) is record
      Call_Chain : GNAT.Traceback.Tracebacks_Array (1 .. Call_Chain_Length);
      Allocations : Natural := 0;
      Deallocations : Natural := 0;
      Allocated : System.Storage_Elements.Storage_Count := 0;
      Deallocated : System.Storage_Elements.Storage_Count := 0;
   end record;
   type Call_Site_Data_P is access all Call_Site_Data;

   function "=" (L, R : Call_Site_Data_P) return Boolean;
   function "<" (L, R : Call_Site_Data_P) return Boolean;

   --  Allocation information, used to keep track of current
   --  allocations so that the call site information can be updated
   --  when the allocation is deallocated.

   type Allocation is record
      Storage_Address : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count;
      Call_Info : Call_Site_Data_P;  -- so we can update on deallocation
   end record;

   function "=" (L, R : Allocation) return Boolean;
   function "<" (L, R : Allocation) return Boolean;

   Tree_Pool : BC.Support.Unmanaged_Storage.Pool;
   --  GNAT 3.16a1 workround
   Pool : System.Storage_Pools.Root_Storage_Pool'Class
     renames System.Storage_Pools.Root_Storage_Pool'Class (Tree_Pool);

   package Abstract_Allocation_Containers
   is new BC.Containers (Allocation);
   package Abstract_Allocation_Trees
   is new Abstract_Allocation_Containers.Trees;
   package Allocation_Trees
   is new Abstract_Allocation_Trees.AVL (Storage => Pool);

   package Abstract_Call_Site_Data_Containers
   is new BC.Containers (Call_Site_Data_P);
   package Abstract_Call_Site_Data_Trees
   is new Abstract_Call_Site_Data_Containers.Trees;
   package Call_Site_Data_Trees
   is new Abstract_Call_Site_Data_Trees.AVL (Storage => Pool);

   type Unbounded_Pool (Limit : System.Storage_Elements.Storage_Count)
   is new ColdFrame.Unbounded_Storage_Pools.Unbounded_Pool with record
      Excluder : Mutex;
      Extent : System.Storage_Elements.Storage_Count := 0;
      Allocations : Allocation_Trees.AVL_Tree;
      Call_Sites : Call_Site_Data_Trees.AVL_Tree;
      Limit_Reached : Boolean := False;
   end record;

end ColdFrame.Unbounded_Debug_Storage_Pools;
