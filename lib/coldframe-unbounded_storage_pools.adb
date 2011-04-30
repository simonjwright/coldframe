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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with System.Memory;

package body ColdFrame.Unbounded_Storage_Pools is


   Big_Endian : constant Boolean
     := System."=" (System.Default_Bit_Order, System.High_Order_First);


   procedure Allocate
     (Pool                     : in out Unbounded_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment                : in System.Storage_Elements.Storage_Count) is

      pragma Unreferenced (Pool);
      pragma Unreferenced (Alignment);

      use type System.Address;

   begin

      --  malloc() returns a result with the maximum alignemnt
      --  requirement for the machine.
      Storage_Address :=
        System.Memory.Alloc (System.Memory.size_t (Size_In_Storage_Elements));

      if Storage_Address = System.Null_Address then
         raise Storage_Error;
      end if;

      --  fill the allocated memory
      declare
         use type System.Storage_Elements.Storage_Offset;
         subtype Storage
         is System.Storage_Elements.Storage_Array
           (0 .. Size_In_Storage_Elements - 1);
         Result_Address : constant System.Address := Storage_Address;
         Result : Storage;
         pragma Import (Ada, Result);
         for Result'Address use Result_Address;
      begin
         if Big_Endian then
            declare
               Filler : constant System.Storage_Elements.Storage_Array (0 .. 7)
                 := (16#de#, 16#ad#, 16#be#, 16#ef#,
                     16#de#, 16#ad#, 16#be#, 16#ef#);
               Remnant : constant System.Storage_Elements.Storage_Offset
                 := Size_In_Storage_Elements mod Filler'Length;
            begin
               for S in 0 .. Size_In_Storage_Elements / Filler'Length - 1 loop
                  Result (S * Filler'Length ..
                            S * Filler'Length + Filler'Length - 1)
                    := Filler;
               end loop;
               Result (Result'Last - Remnant + 1 .. Result'Last)
                 := Filler (0 .. Remnant - 1);
            end;
         else
            declare
               Filler : constant System.Storage_Elements.Storage_Array (0 .. 7)
                 := (16#ef#, 16#be#, 16#ad#, 16#de#,
                     16#ef#, 16#be#, 16#ad#, 16#de#);
               Remnant : constant System.Storage_Elements.Storage_Offset
                 := Size_In_Storage_Elements mod Filler'Length;
            begin
               for S in 0 .. Size_In_Storage_Elements / Filler'Length - 1 loop
                  Result (S * Filler'Length ..
                            S * Filler'Length + Filler'Length - 1)
                    := Filler;
               end loop;
               Result (Result'Last - Remnant + 1 .. Result'Last)
                 := Filler (0 .. Remnant - 1);
            end;
         end if;
      end;

   end Allocate;


   procedure Deallocate
     (Pool                     : in out Unbounded_Pool;
      Storage_Address          : in System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment                : in System.Storage_Elements.Storage_Count) is

      pragma Unreferenced (Pool);
      pragma Unreferenced (Alignment);

   begin

      --  refill the allocated memory
      declare
         use type System.Storage_Elements.Storage_Offset;
         subtype Storage
         is System.Storage_Elements.Storage_Array
           (0 .. Size_In_Storage_Elements - 1);
         Result_Address : constant System.Address := Storage_Address;
         Result : Storage;
         pragma Import (Ada, Result);
         for Result'Address use Result_Address;
      begin
         if Big_Endian then
            declare
               Filler : constant System.Storage_Elements.Storage_Array (0 .. 7)
                 := (16#de#, 16#ad#, 16#de#, 16#ad#,
                     16#de#, 16#ad#, 16#de#, 16#ad#);
               Remnant : constant System.Storage_Elements.Storage_Offset
                 := Size_In_Storage_Elements mod Filler'Length;
            begin
               for S in 0 .. Size_In_Storage_Elements / Filler'Length - 1 loop
                  Result (S * Filler'Length ..
                            S * Filler'Length + Filler'Length - 1)
                    := Filler;
               end loop;
               Result (Result'Last - Remnant + 1 .. Result'Last)
                 := Filler (0 .. Remnant - 1);
            end;
         else
            declare
               Filler : constant System.Storage_Elements.Storage_Array (0 .. 7)
                 := (16#ad#, 16#de#, 16#ad#, 16#de#,
                     16#ad#, 16#de#, 16#ad#, 16#de#);
               Remnant : constant System.Storage_Elements.Storage_Offset
                 := Size_In_Storage_Elements mod Filler'Length;
            begin
               for S in 0 .. Size_In_Storage_Elements / Filler'Length - 1 loop
                  Result (S * Filler'Length ..
                            S * Filler'Length + Filler'Length - 1)
                    := Filler;
               end loop;
               Result (Result'Last - Remnant + 1 .. Result'Last)
                 := Filler (0 .. Remnant - 1);
            end;
         end if;
      end;

      System.Memory.Free (Storage_Address);

   end Deallocate;


   function Storage_Size
     (Pool : Unbounded_Pool)
     return System.Storage_Elements.Storage_Count is
      pragma Unreferenced (Pool);
   begin
      return System.Storage_Elements.Storage_Count'Last;
   end Storage_Size;


end ColdFrame.Unbounded_Storage_Pools;
