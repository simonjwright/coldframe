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
--  * operations are protected against concurrent access.

--  $RCSfile: coldframe-bounded_storage_pools.adb,v $
--  $Revision: ee76e810d3cc $
--  $Date: 2003/07/24 19:28:43 $
--  $Author: simon $

package body ColdFrame.Project.Bounded_Storage_Pools is


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


   procedure Allocate
     (Pool         : in out Fixed_Pool;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count) is
   begin

      --  seize the pool
      Pool.Excluder.Seize;

      --  initialize if necessary
      if not Pool.Initialized then
         Pool.Initialized := True;
         System.Pool_Size.Initialize
           (System.Pool_Size.Stack_Bounded_Pool (Pool));
      end if;

      --  allocate the required storage
      System.Pool_Size.Allocate
        (Pool => System.Pool_Size.Stack_Bounded_Pool (Pool),
         Address => Address,
         Storage_Size => Storage_Size,
         Alignment => Alignment);

      --  release the pool
      Pool.Excluder.Release;

      --  fill the allocated memory
      declare
         use type System.Storage_Elements.Storage_Offset;
         subtype Storage
         is System.Storage_Elements.Storage_Array
           (0 .. Storage_Size - 1);
         Result : Storage;
         for Result'Address use Address;
         Filler : constant System.Storage_Elements.Storage_Array (0 .. 7)
           := (16#d#, 16#e#, 16#A#, 16#d#, 16#b#, 16#e#, 16#e#, 16#f#);
         Remnant : constant System.Storage_Elements.Storage_Offset
           := Storage_Size mod Filler'Length;
      begin
         for S in 0 .. Storage_Size / Filler'Length - 1 loop
            Result (S * Filler'Length .. S * Filler'Length + Filler'Length - 1)
              := Filler;
         end loop;
         Result (Result'Last - Remnant + 1 .. Result'Last)
           := Filler (0 .. Remnant - 1);
      end;

   exception
      when others =>
         --  make sure the pool is released
         Pool.Excluder.Release;
         raise;
   end Allocate;


   procedure Deallocate
     (Pool         : in out Fixed_Pool;
      Address      : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count) is
   begin

      --  seize the pool
      Pool.Excluder.Seize;

      --  deallocate the storage
      System.Pool_Size.Deallocate
        (Pool => System.Pool_Size.Stack_Bounded_Pool (Pool),
         Address => Address,
         Storage_Size => Storage_Size,
         Alignment => Alignment);

      --  release the pool
      Pool.Excluder.Release;

   exception
      when others =>
         --  make sure the pool is released
         Pool.Excluder.Release;
         raise;
   end Deallocate;


   function Storage_Size
     (Pool : Fixed_Pool)
     return System.Storage_Elements.Storage_Count is
   begin
      return System.Pool_Size.Storage_Size
        (System.Pool_Size.Stack_Bounded_Pool (Pool));
   end Storage_Size;


end ColdFrame.Project.Bounded_Storage_Pools;
