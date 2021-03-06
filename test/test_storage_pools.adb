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

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with ColdFrame.Project.Storage_Pools;
with Interfaces;
with System.Storage_Elements;

procedure Test_Storage_Pools is

begin

   Put_Line ("Bounded_Pool tests.");

   for Size in System.Storage_Elements.Storage_Offset'(0) .. 17 loop

      declare

         type T is new System.Storage_Elements.Storage_Array (1 .. Size);

         T_Pool : ColdFrame.Project.Storage_Pools.Bounded_Pool
           (Pool_Size => 1024,
            Elmt_Size => T'Max_Size_In_Storage_Elements,
            Alignment => T'Alignment);
         pragma Warnings (Off, T_Pool);

         type T_P is access T;
         for T_P'Storage_Pool use T_Pool;
         procedure Free is new Ada.Unchecked_Deallocation (T, T_P);

         P : T_P;
         Dangling_P : T_P;

      begin

         P := new T;

         Put_Line ("size is" & Size'Img);

         for I in P.all'Range loop
            declare
               Tmp : String (1 .. 6);
            begin
               Put (Tmp, Integer (P (I)), Base => 16);
               Put (Tmp (4 .. 5));
            end;
         end loop;
         New_Line;

         Dangling_P := P;
         P.all := (others => 0);
         Free (P);
         for I in Dangling_P.all'Range loop
            declare
               Tmp : String (1 .. 6);
            begin
               Put (Tmp, Integer (Dangling_P (I)), Base => 16);
               Put (Tmp (4 .. 5));
            end;
         end loop;
         New_Line;

      end;

   end loop;

   New_Line;
   Put_Line ("Unbounded Pool tests.");

   for Size in System.Storage_Elements.Storage_Offset'(0) .. 17 loop

      declare

         type T is new System.Storage_Elements.Storage_Array (1 .. Size);

         type T_P is access T;
         for T_P'Storage_Pool use ColdFrame.Project.Storage_Pools.Pool;
         procedure Free is new Ada.Unchecked_Deallocation (T, T_P);

         P : T_P;
         Dangling_P : T_P;

      begin

         P := new T;

         Put_Line ("size is" & Size'Img);

         for I in P.all'Range loop
            declare
               Tmp : String (1 .. 6);
            begin
               Put (Tmp, Integer (P (I)), Base => 16);
               Put (Tmp (4 .. 5));
            end;
         end loop;
         New_Line;

         Dangling_P := P;
         P.all := (others => 0);
         Free (P);
         for I in Dangling_P.all'Range loop
            declare
               Tmp : String (1 .. 6);
            begin
               Put (Tmp, Integer (Dangling_P (I)), Base => 16);
               Put (Tmp (4 .. 5));
            end;
         end loop;
         New_Line;

      end;

   end loop;

   --  When looking at data with the debugger you tend to see fills
   --  best when printing inb hex (print/x), and of course the result
   --  is endian-dependent. Make sure that the data looks good:

   New_Line;
   Put_Line ("Debugger view (Bounded_Pool).");

   declare

      type T is new Interfaces.Unsigned_64;

      package T_IO is new Modular_IO (T);

      T_Pool : ColdFrame.Project.Storage_Pools.Bounded_Pool
        (Pool_Size => 128,
         Elmt_Size => T'Max_Size_In_Storage_Elements,
         Alignment => T'Alignment);
      pragma Warnings (Off, T_Pool);

      type T_P is access T;
      for T_P'Storage_Pool use T_Pool;
      procedure Free is new Ada.Unchecked_Deallocation (T, T_P);

      P : T_P;
      Dangling_P : T_P;

   begin

      P := new T;

      T_IO.Put (P.all, Base => 16);
      New_Line;

      Dangling_P := P;
      P.all := 0;
      Free (P);
      T_IO.Put (Dangling_P.all, Base => 16);
      New_Line;

   end;

   New_Line;
   Put_Line ("Debugger view (unbounded Pool).");

   declare

      type T is new Interfaces.Unsigned_64;

      package T_IO is new Modular_IO (T);

      type T_P is access T;
      for T_P'Storage_Pool use ColdFrame.Project.Storage_Pools.Pool;
      procedure Free is new Ada.Unchecked_Deallocation (T, T_P);

      P : T_P;
      Dangling_P : T_P;

   begin

      P := new T;

      T_IO.Put (P.all, Base => 16);
      New_Line;

      Dangling_P := P;
      P.all := 0;
      Free (P);
      T_IO.Put (Dangling_P.all, Base => 16);
      New_Line;

   end;

end Test_Storage_Pools;
