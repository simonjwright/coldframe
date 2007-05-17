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

--  $RCSfile: coldframe-unbounded_debug_storage_pools.adb,v $
--  $Revision: f80c44ac3b2a $
--  $Date: 2007/05/17 21:40:30 $
--  $Author: simonjwright $

with Ada.Text_IO;
with GNAT.Debug_Utilities;

package body ColdFrame.Unbounded_Debug_Storage_Pools is


   package CUSP renames ColdFrame.Unbounded_Storage_Pools;


   procedure Allocate
     (Pool                     : in out Unbounded_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment                : in System.Storage_Elements.Storage_Count) is
      This_Allocation : Allocation;
      Allocation_Not_Found : Boolean;
      use type System.Storage_Elements.Storage_Count;
   begin
      Pool.Excluder.Seize;
      CUSP.Allocate (CUSP.Unbounded_Pool (Pool),
                     Storage_Address,
                     Size_In_Storage_Elements,
                     Alignment);
      This_Allocation.Storage_Address := Storage_Address;
      This_Allocation.Size_In_Storage_Elements := Size_In_Storage_Elements;
      This_Allocation.Alignment := Alignment;
      GNAT.Traceback.Call_Chain (This_Allocation.Call_Chain,
                                 This_Allocation.Call_Chain_Length);
      Trees.Insert (Pool.Current_Allocations,
                    This_Allocation,
                    Allocation_Not_Found);
      pragma Assert (Allocation_Not_Found, "allocation already present");
      Pool.Current_Allocation :=
        Pool.Current_Allocation + Size_In_Storage_Elements;
      if Pool.Current_Allocation > Pool.Limit
        and then not Pool.Limit_Reached
      then
         Pool.Limit_Reached := True;
         Report (Pool, "debug_storage_pool.log");
      end if;
      Pool.Excluder.Release;
   exception
      when others =>
         Pool.Excluder.Release;
   end Allocate;


   procedure Deallocate
     (Pool                     : in out Unbounded_Pool;
      Storage_Address          : in System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment                : in System.Storage_Elements.Storage_Count) is
      Allocation_Found : Boolean;
   begin
      Pool.Excluder.Seize;
      CUSP.Deallocate (CUSP.Unbounded_Pool (Pool),
                       Storage_Address,
                       Size_In_Storage_Elements,
                       Alignment);
      Trees.Delete (Pool.Current_Allocations,
                    Allocation'(Storage_Address,
                                Size_In_Storage_Elements,
                                Alignment,
                                Tracebacks_Array'
                                  (others => System.Null_Address),
                                0),
                    Allocation_Found);
      Pool.Excluder.Release;
      pragma Assert (Allocation_Found, "allocation not found");
   exception
      when others =>
         Pool.Excluder.Release;
   end Deallocate;


   function Storage_Size
     (Pool : Unbounded_Pool)
     return System.Storage_Elements.Storage_Count is
   begin
      return CUSP.Storage_Size (CUSP.Unbounded_Pool (Pool));
   end Storage_Size;


   procedure Report
     (Pool          : Unbounded_Pool;
      To_File_Named : String) is
      procedure Print (Item : Allocation; OK : out Boolean);
      procedure Print is new Trees.Visit (Print);
      F : Ada.Text_IO.File_Type;
      procedure Print (Item : Allocation; OK : out Boolean) is
         Result : String (1 .. 1024);
         Last : Natural := 0;
      begin
         OK := True;
         for C in 1 .. Item.Call_Chain_Length loop
            declare
               Site : constant String
                 := GNAT.Debug_Utilities.Image (Item.Call_Chain (C));
            begin
               Last := Last + 3;
               Result (Last - 2 .. Last) := " 0x";
               for S in Site'First + 3 .. Site'Last loop
                  case Site (S) is
                     when 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' =>
                        Last := Last + 1;
                        Result (Last) := Site (S);
                     when others =>
                        null;
                  end case;
               end loop;
            end;
         end loop;
         Ada.Text_IO.Put_Line (F, Result (1 .. Last));
      end Print;
   begin
      begin
         Ada.Text_IO.Open (F,
                           Name => To_File_Named,
                           Mode => Ada.Text_IO.Out_File);
      exception
         when Ada.Text_IO.Name_Error =>
            Ada.Text_IO.Create (F,
                                Name => To_File_Named);
      end;
      Print (Pool.Current_Allocations);
      Ada.Text_IO.Close (F);
   end Report;


   protected body Mutex is
      entry Seize when not Seized is
      begin
         Seized := True;
      end Seize;
      procedure Release is
      begin
         Seized := False;
      end Release;
   end Mutex;


   function "=" (L, R : Allocation) return Boolean is
      use type System.Address;
   begin
      return L.Storage_Address = R.Storage_Address;
   end "=";


   function "<" (L, R : Allocation) return Boolean is
      use type System.Storage_Elements.Integer_Address;
   begin
      return System.Storage_Elements.To_Integer (L.Storage_Address) <
        System.Storage_Elements.To_Integer (R.Storage_Address);
   end "<";


end ColdFrame.Unbounded_Debug_Storage_Pools;
