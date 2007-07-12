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
--  $Revision: 7a9dfe80e94f $
--  $Date: 2007/07/12 21:12:27 $
--  $Author: simonjwright $

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Debug_Utilities;

package body ColdFrame.Unbounded_Debug_Storage_Pools is


   package CUSP renames ColdFrame.Unbounded_Storage_Pools;


   procedure Allocate
     (Pool                     : in out Unbounded_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment                : in System.Storage_Elements.Storage_Count)
   is
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
      declare
         Call_Chain : GNAT.Traceback.Tracebacks_Array
           (1 .. Max_Stored_Call_Chain_Length);
         Last : Natural;
      begin
         GNAT.Traceback.Call_Chain (Call_Chain, Last);
         declare
            procedure Update_Existing_Call_Site_Data
              (Call_Site : in out Call_Site_Data_P);
            procedure Update_Existing_Call_Site_Data
            is new Call_Site_Data_Trees.Access_Actual_Item
              (Update_Existing_Call_Site_Data);
            procedure Update_Existing_Call_Site_Data
              (Call_Site : in out Call_Site_Data_P) is
            begin
               This_Allocation.Call_Info := Call_Site;
               Call_Site.Allocations := Call_Site.Allocations + 1;
               Call_Site.Allocated :=
                 Call_Site.Allocated + Size_In_Storage_Elements;
            end Update_Existing_Call_Site_Data;
            Dummy_Call_Site_Data : aliased Call_Site_Data
              := Call_Site_Data'(Last,
                                 Call_Chain (1 .. Last),
                                 Allocations => 1,
                                 Deallocations => 0,
                                 Allocated => Size_In_Storage_Elements,
                                 Deallocated => 0);
            Actual_Found, Actual_Not_Found : Boolean;
         begin
            Update_Existing_Call_Site_Data
              (Pool.Call_Sites,
               Dummy_Call_Site_Data'Unchecked_Access,
               Actual_Found);
            if not Actual_Found then
               This_Allocation.Call_Info :=
                 new Call_Site_Data'(Last,
                                     Call_Chain (1 .. Last),
                                     Allocations => 1,
                                     Deallocations => 0,
                                     Allocated => Size_In_Storage_Elements,
                                     Deallocated => 0);
               Call_Site_Data_Trees.Insert
                 (Pool.Call_Sites,
                  This_Allocation.Call_Info,
                  Actual_Not_Found);
               pragma Assert (Actual_Not_Found,
                              "call site data already present");
            end if;
         end;
      end;
      Allocation_Trees.Insert (Pool.Allocations,
                               This_Allocation,
                               Allocation_Not_Found);
      pragma Assert (Allocation_Not_Found, "allocation already present");
      Pool.Extent := Pool.Extent + Size_In_Storage_Elements;
      if Pool.Extent > Pool.Limit
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
      Alignment                : in System.Storage_Elements.Storage_Count)
   is
      procedure Update_Call_Site_Data (A : in out Allocation);
      procedure Update_Call_Site_Data
      is new Allocation_Trees.Access_Actual_Item (Update_Call_Site_Data);
      use type System.Storage_Elements.Storage_Count;
      procedure Update_Call_Site_Data (A : in out Allocation) is
      begin
         A.Call_Info.Deallocations := A.Call_Info.Deallocations + 1;
         A.Call_Info.Deallocated :=
           A.Call_Info.Deallocated + Size_In_Storage_Elements;
      end Update_Call_Site_Data;
      Dummy_Allocation : constant Allocation
        := Allocation'(Storage_Address,
                       Size_In_Storage_Elements,
                       Alignment,
                       null);
      Allocation_Found : Boolean;
   begin
      Pool.Excluder.Seize;
      Pool.Extent := Pool.Extent - Size_In_Storage_Elements;
      CUSP.Deallocate (CUSP.Unbounded_Pool (Pool),
                       Storage_Address,
                       Size_In_Storage_Elements,
                       Alignment);
      Update_Call_Site_Data (Pool.Allocations,
                             Dummy_Allocation,
                             Allocation_Found);
      pragma Assert (Allocation_Found, "allocation to update not found");
      Allocation_Trees.Delete (Pool.Allocations,
                               Dummy_Allocation,
                               Allocation_Found);
      pragma Assert (Allocation_Found, "allocation to delete not found");
      Pool.Excluder.Release;
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
      procedure Print (Item : Call_Site_Data_P; OK : out Boolean);
      procedure Print is new Call_Site_Data_Trees.Visit (Print);
      F : Ada.Text_IO.File_Type;
      procedure Print (Item : Call_Site_Data_P; OK : out Boolean) is
         Result : String (1 .. 1024);
         Last : Natural := 0;
      begin
         OK := True;
         Put (F, Item.Allocations'Img);
         Put (F, ',');
         Put (F, Item.Deallocations'Img);
         Put (F, ',');
         Put (F, Item.Allocated'Img);
         Put (F, ',');
         Put (F, Item.Deallocated'Img);
         Put (F, ',');
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
         Put_Line (F, Result (1 .. Last));
      end Print;
   begin
      begin
         Open (F, Name => To_File_Named, Mode => Out_File);
      exception
         when Name_Error =>
            Create (F, Name => To_File_Named);
      end;
      Put_Line
        (F, "Allocations,Deallocations,Allocated,Deallocated,Call_Chain");
      Print (Pool.Call_Sites);
      Close (F);
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


   function "=" (L, R : Call_Site_Data_P) return Boolean is
      use type GNAT.Traceback.Tracebacks_Array;
   begin
      return L.Call_Chain = R.Call_Chain;
   end "=";


   function "<" (L, R : Call_Site_Data_P) return Boolean is
      use type System.Storage_Elements.Integer_Address;
   begin
      if L.Call_Chain_Length < R.Call_Chain_Length then
         for C in L.Call_Chain'Range loop
            declare
               R_Index : constant Positive :=
                 C - L.Call_Chain'First + R.Call_Chain'First;
               L_A : System.Storage_Elements.Integer_Address renames
                 System.Storage_Elements.To_Integer (L.Call_Chain (C));
               R_A : System.Storage_Elements.Integer_Address renames
                 System.Storage_Elements.To_Integer (R.Call_Chain (R_Index));
            begin
               if L_A < R_A then
                  return True;
               elsif L_A > R_A then
                  return False;
               end if;
            end;
         end loop;
         return True;
      elsif L.Call_Chain_Length = R.Call_Chain_Length then
         for C in L.Call_Chain'Range loop
            declare
               R_Index : constant Positive :=
                 C - L.Call_Chain'First + R.Call_Chain'First;
               L_A : System.Storage_Elements.Integer_Address renames
                 System.Storage_Elements.To_Integer (L.Call_Chain (C));
               R_A : System.Storage_Elements.Integer_Address renames
                 System.Storage_Elements.To_Integer (R.Call_Chain (R_Index));
            begin
               if L_A < R_A then
                  return True;
               elsif L_A > R_A then
                  return False;
               end if;
            end;
         end loop;
         return False;
      else
         for C in R.Call_Chain'Range loop
            declare
               L_Index : constant Positive :=
                 C - R.Call_Chain'First + L.Call_Chain'First;
               L_A : System.Storage_Elements.Integer_Address renames
                 System.Storage_Elements.To_Integer (L.Call_Chain (L_Index));
               R_A : System.Storage_Elements.Integer_Address renames
                 System.Storage_Elements.To_Integer (R.Call_Chain (C));
            begin
               if L_A < R_A then
                  return True;
               elsif L_A > R_A then
                  return False;
               end if;
            end;
         end loop;
         return False;
      end if;
   end "<";


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
