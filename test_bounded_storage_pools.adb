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

--  $RCSfile: test_bounded_storage_pools.adb,v $
--  $Revision: d515d80b9067 $
--  $Date: 2003/08/23 07:41:22 $
--  $Author: simon $

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with ColdFrame.Project.Storage_Pools;
with System.Storage_Elements;

procedure Test_Bounded_Storage_Pools is

   type T is new System.Storage_Elements.Storage_Array (1 .. 17);

   T_Pool : ColdFrame.Project.Storage_Pools.Bounded_Pool
     (Pool_Size => 1024,
      Elmt_Size => T'Max_Size_In_Storage_Elements,
      Alignment => T'Alignment);
   pragma Warnings (Off, T_Pool);

   type T_P is access T;
   --     for T_P'Storage_Pool use ColdFrame.Project.Storage_Pools.Pool;
   for T_P'Storage_Pool use T_Pool;

   P : T_P;

begin

   P := new T;

   for I in P.all'Range loop
      Put (Integer (P (I)), Base => 16);
      New_Line;
   end loop;

end Test_Bounded_Storage_Pools;
