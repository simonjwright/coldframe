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

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile: coldframe-string_hash.adb,v $
--  $Revision: fe0f9536cdc0 $
--  $Date: 2001/09/25 18:39:48 $
--  $Author: simon $

with Ada.Numerics.Discrete_Random;

package body ColdFrame.String_Hash is


   type Special_Integer is mod 2 ** 32;
   Character_Hash : array (Character) of Special_Integer;


   procedure Init (Seed : Integer := 10009);

   procedure Init (Seed : Integer := 10009) is
      package Random_Integer is new
        Ada.Numerics.Discrete_Random (Result_Subtype => Special_Integer);
      Generator : Random_Integer.Generator;
   begin
      Random_Integer.Reset (Gen => Generator, Initiator => Seed);
      for K in Character_Hash'Range loop
         Character_Hash (K) :=
           Random_Integer.Random (Gen => Generator);
      end loop;
   end Init;


   function Hash
     (S : String; Hash_Table_Size : in Integer := 43) return Integer is
      K : Special_Integer := 0;
      N : Special_Integer := 0;
   begin

      if S = "" then
         return 0;
      end if;

      for M in S'Range loop
         N := Character_Hash (S (M));
         K := K + Special_Integer (M) * N;
      end loop;

      return Integer (K) mod Hash_Table_Size;

   end Hash;


   function Bounded_Hash (S : Bounded.Bounded_String;
                          Hash_Table_Size : in Integer := 43)
                         return Integer is
      K : Special_Integer := 0;
      N : Special_Integer := 0;
      use Bounded;
   begin

      for M in 1 .. Length (S) loop
         N := Character_Hash (Element (S, M));
         K := K + Special_Integer (M) * N;
      end loop;

      return Integer (K) mod Hash_Table_Size;

   end Bounded_Hash;


   function Hash (S : Ada.Strings.Unbounded.Unbounded_String;
                  Hash_Table_Size : in Integer := 43)
                 return Integer is
      K : Special_Integer := 0;
      N : Special_Integer := 0;
      use Ada.Strings.Unbounded;
   begin

      for M in 1 .. Length (S) loop
         N := Character_Hash (Element (S, M));
         K := K + Special_Integer (M) * N;
      end loop;

      return Integer (K) mod Hash_Table_Size;

   end Hash;


begin

   Init;

end ColdFrame.String_Hash;
