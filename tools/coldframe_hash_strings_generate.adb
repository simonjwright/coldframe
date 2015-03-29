--  Copyright (C) Simon Wright <simon@pushface.org>

--  Algorithm due to Donald Knuth; from an implementation by Daniel
--  Gaudry <Daniel.Gaudry@wanadoo.fr>

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

--  This program generates the Character_Hash table in the private
--  part of ColdFrame.Hash.Strings (a one-off, included for reference).

with Ada.Containers;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;

procedure ColdFrame_Hash_Strings_Generate is


   procedure Init (Seed : Integer := 10009);


   CH : array (Character) of Ada.Containers.Hash_Type;

   use type Ada.Containers.Hash_Type;


   procedure Init (Seed : Integer := 10009) is
      package Random_Hash is new
        Ada.Numerics.Discrete_Random
          (Result_Subtype => Ada.Containers.Hash_Type);
      Generator : Random_Hash.Generator;
   begin
      Random_Hash.Reset (Gen => Generator, Initiator => Seed);
      for K in CH'Range loop
         CH (K) :=
           Random_Hash.Random (Gen => Generator);
      end loop;
   end Init;


begin

   Init;

   for K in CH'Range loop
      if Character'Pos (K) /= 0 and Character'Pos (K) mod 4 = 0 then
         New_Line;
      end if;
      Put (CH (K)'Img & ',');
   end loop;

   New_Line;

end ColdFrame_Hash_Strings_Generate;
