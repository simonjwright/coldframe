-- Copyright (C) Simon Wright <simon@pushface.org>

-- This package is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2, or (at
-- your option) any later version. This package is distributed in the
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even
-- the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE. See the GNU General Public License for more details. You
-- should have received a copy of the GNU General Public License
-- distributed with this package; see file COPYING.  If not, write to
-- the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
-- MA 02111-1307, USA.

-- $Id: coldframe-test_string_hash.adb,v bf8a0244a396 2001/01/12 20:35:01 simon $

with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Ada.Text_Io;
with Architecture.String_Hash;

procedure Architecture.Test_String_Hash is

  package S128 is new Ada.Strings.Bounded.Generic_Bounded_Length (128);
  function S128_Hash is new Architecture.String_Hash.Bounded_Hash (S128);

  procedure T (S : String) is
    use Architecture.String_Hash;
    use S128;
    use Ada.Strings.Unbounded;
  begin
    Ada.Text_Io.Put_Line ("hashing |" & S & "|:");
    Ada.Text_Io.Put_Line
       ("  plain     ->"
        & Integer'Image (Hash (S, 10000)));
    Ada.Text_Io.Put_Line
       ("  bounded   ->"
        & Integer'Image (S128_Hash (To_Bounded_String (S), 10000)));
    Ada.Text_Io.Put_Line
       ("  unbounded ->"
        & Integer'Image (Hash (To_Unbounded_String (S), 10000)));
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
end Architecture.Test_String_Hash;
