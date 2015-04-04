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

with Digital_IO_Support;
private with Ada.Unchecked_Conversion;
private with Interfaces;

package Digital_IO.STM32F4_Support is

   procedure Initialize;

private

   type Implementation
     is new Digital_IO_Support.Implementation with null record;

   overriding
   function Get (This      : Implementation;
                 For_Input : Digital_IO_Support.Input_Signal) return Boolean;

   overriding
   procedure Set (This       : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To         : Boolean);

   --  Types for bit/partword handling.

   type Bits_1 is mod 2 with  Size => 1;
   type Bits_2 is mod 4 with  Size => 2;
   type Bits_4 is mod 16 with Size => 4;

   type Bits_16x1 is array (0 .. 15) of Bits_1 with Pack, Size => 16;
   type Bits_16x2 is array (0 .. 7)  of Bits_2 with Pack, Size => 16;
   type Bits_16x4 is array (0 .. 3)  of Bits_4 with Pack, Size => 16;

   function U16_To_B1
     is new Ada.Unchecked_Conversion (Interfaces.Unsigned_16, Bits_16x1);
   function B1_To_U16
     is new Ada.Unchecked_Conversion (Bits_16x1, Interfaces.Unsigned_16);
   function U16_To_B2
     is new Ada.Unchecked_Conversion (Interfaces.Unsigned_16, Bits_16x2);
   function B2_To_U16
     is new Ada.Unchecked_Conversion (Bits_16x2, Interfaces.Unsigned_16);
   function U16_To_B4
     is new Ada.Unchecked_Conversion (Interfaces.Unsigned_16, Bits_16x4);
   function B4_To_U16
     is new Ada.Unchecked_Conversion (Bits_16x4, Interfaces.Unsigned_16);

   type Bits_32x1 is array (0 .. 31) of Bits_1 with Pack, Size => 32;
   type Bits_32x2 is array (0 .. 15) of Bits_2 with Pack, Size => 32;
   type Bits_32x4 is array (0 .. 7)  of Bits_4 with Pack, Size => 32;

   function U32_To_B1
     is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Bits_32x1);
   function B1_To_U32
     is new Ada.Unchecked_Conversion (Bits_32x1, Interfaces.Unsigned_32);
   function U32_To_B2
     is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Bits_32x2);
   function B2_To_U32
     is new Ada.Unchecked_Conversion (Bits_32x2, Interfaces.Unsigned_32);
   function U32_To_B4
     is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Bits_32x4);
   function B4_To_U32
     is new Ada.Unchecked_Conversion (Bits_32x4, Interfaces.Unsigned_32);

end Digital_IO.STM32F4_Support;
