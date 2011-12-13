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

--  $RCSfile: normalize_xmi-identifiers.adb,v $
--  $Revision: 2e42ac7f6e38 $
--  $Date: 2011/12/13 17:12:41 $
--  $Author: simonjwright $

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Normalize_XMI.Errors;

package body Normalize_XMI.Identifiers is

   function Casing_Equals (L, R : String) return Boolean;
   function Casing_Less_Than (L, R : String) return Boolean;

   package Lowercase_String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => String,
      Element_Type => String,
      "<" => Casing_Less_Than,
      "=" => Casing_Equals);

   --  For Case_Exceptions, the whole identifier has to match.
   Case_Exceptions : Lowercase_String_Maps.Map;
   --  For Sub_Case_Exceptions, each 'word' in the identifier matches
   --  separately.
   Sub_Case_Exceptions : Lowercase_String_Maps.Map;

   package Lowercase_String_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => String,
      "<" => Casing_Less_Than,
      "=" => Casing_Equals);

   Reserved : Lowercase_String_Sets.Set;

   function Casing_Equals (L, R : String) return Boolean
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
   begin
      return Translate (L, Lower_Case_Map) = Translate (R, Lower_Case_Map);
   end Casing_Equals;


   function Casing_Less_Than (L, R : String) return Boolean
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
   begin
      return Translate (L, Lower_Case_Map) < Translate (R, Lower_Case_Map);
   end Casing_Less_Than;


   type Span is record
      L : Natural;
      U : Natural := 0;
   end record;
   type Spans is array (Natural range <>) of Span;

   function Find_Spans (S : String; Splitting_At : Character) return Spans;


   procedure Read_Case_Exceptions (From : String)
   is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "... not reading case exceptions from " & From);
      --  We can use GNAT.Directory_Operations.Dir_Separator.
      Sub_Case_Exceptions.Insert ("io", "IO");
   end Read_Case_Exceptions;


   --  Maps upper to lower case, and _ to space.
   Lowercase_Space_Map : Ada.Strings.Maps.Character_Mapping;

   function Normalize (Id : String) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
      use Ada.Strings.Maps.Constants;

      --  A Component consists of lower-case space-separated Words,
      --  each of which is subjected to Sub_Case_Exceptions (such as
      --  "IO" in "Text_IO", or, if no exception is found, the first
      --  character is capitalized.
      procedure Process_Component (S : in out String);
      procedure Process_Word (S : in out String);

      procedure Process_Component (S : in out String)
      is
         Words : constant Spans := Find_Spans (S, ' ');
      begin
         if Reserved.Contains (S) then
            Errors.Report
              ("Reserved word """ & S & """ not allowed.");
         end if;
         for W in Words'Range loop
            Process_Word (S (Words (W).L .. Words (W).U));
         end loop;
         Translate (S, To_Mapping (" ", "_"));
         --  Finally, we look for whole-component exceptions (such as
         --  "unsigned_short").
         declare
            Lower : constant String := Translate (S, Lower_Case_Map);
         begin
            if Case_Exceptions.Contains (Lower) then
               S := Case_Exceptions.Element (Lower);
            end if;
         end;
      end Process_Component;

      procedure Process_Word (S : in out String)
      is
      begin
         if Sub_Case_Exceptions.Contains (S) then
            S := Sub_Case_Exceptions.Element (S);
         elsif S'Length > 0 then
            Translate (S (S'First .. S'First), Upper_Case_Map);
         end if;
      end Process_Word;

      --  Convert to lower-case, entirely space-separated.
      Result : String := Translate (Trim (Id, Both), Lowercase_Space_Map);

      --  Find the dot-separated components
      Components : constant Spans := Find_Spans (Result, '.');

   begin
      for C in Components'Range loop
         Process_Component (Result (Components (C).L .. Components (C).U));
      end loop;
      return Result;
   end Normalize;


   function Find_Spans (S : String; Splitting_At : Character) return Spans
   is
      use Ada.Strings.Fixed;
      Result : Spans (1 .. Count (S, String'(1 => Splitting_At)) + 1);
      J : Positive := 1;
   begin
      Result (J).L := S'First;
      for K in S'Range loop
         if S (K) = Splitting_At then
            Result (J).U := K - 1;
            J := J + 1;
            Result (J).L := K + 1;
         elsif K = S'Last then
            Result (J).U := K;
         end if;
      end loop;
      return Result;
   end Find_Spans;


begin

   declare
      use Ada.Strings.Maps;
      Map_Domain : constant Character_Sequence
        := To_Domain (Constants.Lower_Case_Map);
      Map_Range : constant Character_Sequence
        := To_Range (Constants.Lower_Case_Map);
   begin
      Lowercase_Space_Map := To_Mapping (From => Map_Domain & '_',
                                         To => Map_Range & ' ');
   end;

   --  Save the reserved words (of Ada 95).
   Reserved.Insert ("abort");
   Reserved.Insert ("abs");
   Reserved.Insert ("abstract");
   Reserved.Insert ("accept");
   Reserved.Insert ("access");
   Reserved.Insert ("aliased");
   Reserved.Insert ("all");
   Reserved.Insert ("and");
   Reserved.Insert ("array");
   Reserved.Insert ("at");
   Reserved.Insert ("begin");
   Reserved.Insert ("body");
   Reserved.Insert ("case");
   Reserved.Insert ("constant");
   Reserved.Insert ("declare");
   Reserved.Insert ("delay");
   Reserved.Insert ("delta");
   Reserved.Insert ("digits");
   Reserved.Insert ("do");
   Reserved.Insert ("else");
   Reserved.Insert ("elsif");
   Reserved.Insert ("end");
   Reserved.Insert ("entry");
   Reserved.Insert ("exception");
   Reserved.Insert ("exit");
   Reserved.Insert ("for");
   Reserved.Insert ("function");
   Reserved.Insert ("generic");
   Reserved.Insert ("goto");
   Reserved.Insert ("if");
   Reserved.Insert ("in");
   Reserved.Insert ("is");
   Reserved.Insert ("limited");
   Reserved.Insert ("loop");
   Reserved.Insert ("mod");
   Reserved.Insert ("new");
   Reserved.Insert ("not");
   Reserved.Insert ("null");
   Reserved.Insert ("of");
   Reserved.Insert ("or");
   Reserved.Insert ("others");
   Reserved.Insert ("out");
   Reserved.Insert ("package");
   Reserved.Insert ("pragma");
   Reserved.Insert ("private");
   Reserved.Insert ("procedure");
   Reserved.Insert ("protected");
   Reserved.Insert ("raise");
   Reserved.Insert ("range");
   Reserved.Insert ("record");
   Reserved.Insert ("rem");
   Reserved.Insert ("renames");
   Reserved.Insert ("requeue");
   Reserved.Insert ("return");
   Reserved.Insert ("reverse");
   Reserved.Insert ("select");
   Reserved.Insert ("separate");
   Reserved.Insert ("subtype");
   Reserved.Insert ("tagged");
   Reserved.Insert ("task");
   Reserved.Insert ("terminate");
   Reserved.Insert ("then");
   Reserved.Insert ("type");
   Reserved.Insert ("until");
   Reserved.Insert ("use");
   Reserved.Insert ("when");
   Reserved.Insert ("while");
   Reserved.Insert ("with");
   Reserved.Insert ("xor");

   --  Include the reserved words new in Ada 2005.
   Reserved.Insert ("interface");
   Reserved.Insert ("overriding");
   Reserved.Insert ("synchronized");

end Normalize_XMI.Identifiers;
