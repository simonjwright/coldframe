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

--  $RCSfile: coldframe-hash-strings-test.adb,v $
--  $Revision: 3f8478ad4759 $
--  $Date: 2001/09/28 04:50:58 $
--  $Author: simon $

with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with ColdFrame.Hash.Strings.Standard;
with ColdFrame.Hash.Strings.Bounded;
with ColdFrame.Hash.Strings.Unbounded;

procedure ColdFrame.Hash.Strings.Test is

   package S128 is new Ada.Strings.Bounded.Generic_Bounded_Length (128);
   function S128_Hash is new ColdFrame.Hash.Strings.Bounded (S128);

   procedure T (S : String);

   procedure T (S : String) is
      use S128;
      use Ada.Strings.Unbounded;
   begin
      Ada.Text_IO.Put_Line ("hashing |" & S & "|:");
      Ada.Text_IO.Put_Line
        ("  plain     ->"
         & Natural'Image (ColdFrame.Hash.Strings.Standard (S)));
      Ada.Text_IO.Put_Line
        ("  bounded   ->"
         & Natural'Image (S128_Hash (To_Bounded_String (S))));
      Ada.Text_IO.Put_Line
        ("  unbounded ->"
         & Natural'Image (ColdFrame.Hash.Strings.Unbounded
                          (To_Unbounded_String (S))));
   end T;

begin
   T ("");
   T ("0");
   T ("0");
   T ("a");
   T ("A");
   T ("aa");
   T ("aA");
   T ("b");
   T ("B");
   T ("bb");
   T ("bB");
   T ("now is the time for all good men to come to the aid of the party");
   T ("now is the time for all good men to come to the aid of the part");
   T ("Now is the time for all good men to come to the aid of the party");
   T ("now is the time for all good men to come to the aid of the Party");
end ColdFrame.Hash.Strings.Test;
