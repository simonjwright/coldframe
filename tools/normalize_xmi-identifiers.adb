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
--  $Revision: af0b243fc933 $
--  $Date: 2011/12/29 12:39:49 $
--  $Author: simonjwright $

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Normalize_XMI.Messages;

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
      procedure Add_Exception (From_Line : String);
      procedure Add_Exception (From_Line : String)
      is
         use Ada.Strings;
         use Ada.Strings.Fixed;
         use Ada.Strings.Maps;
         L : constant String := Trim (From_Line, Both);
         --  Find_Spans splits on Character, so translate any tabs to
         --  spaces before splitting.
         Words : constant Spans
           := Find_Spans (Translate (L, To_Mapping ((1 => ASCII.HT), " ")),
                          Splitting_At => ' ');
      begin
         if Words'Length > 0 then
            if L (Words (1).L) /= '#' then
               if L (Words (1).L) = '*' then
                  declare
                     Word : constant String
                       := L (Words (1).L + 1 .. Words (1).U);
                  begin
                     if Sub_Case_Exceptions.Contains (Word) then
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           "Sub-case exception already found for '"
                             & Word
                             & "'");
                     else
                        Sub_Case_Exceptions.Insert (Word, Word);
                     end if;
                  end;
               else
                  declare
                     Word : constant String
                       := L (Words (1).L .. Words (1).U);
                  begin
                     if Case_Exceptions.Contains (Word) then
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           "Case exception already found for '"
                             & Word
                             & "'");
                     else
                        Case_Exceptions.Insert (Word, Word);
                     end if;
                  end;
               end if;
            end if;
         end if;
      end Add_Exception;
      Files : constant Spans
        := Find_Spans (From, GNAT.OS_Lib.Path_Separator);
      use Ada.Text_IO;
   begin
      for F in Files'Range loop
         declare
            Name : constant String := From (Files (F).L .. Files (F).U);
            Exceptions : File_Type;
         begin
            Put_Line (Standard_Error,
                      "reading case exceptions from '" & Name & "'");
            Open (Exceptions, Mode => In_File, Name => Name);
            while not End_Of_File (Exceptions) loop
               Add_Exception (Get_Line (Exceptions));
            end loop;
            Close (Exceptions);
         exception
            when Mode_Error | Name_Error =>
               Put_Line (Standard_Error,
                         "Unable to open case exceptions file '"
                           & Name
                           & "'");
               raise;
         end;
      end loop;
   end Read_Case_Exceptions;


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
            Messages.Error
              ("Reserved word """ & S & """ not allowed.");
         end if;
         for W in Words'Range loop
            Process_Word (S (Words (W).L .. Words (W).U));
         end loop;
         Translate (S, To_Mapping (" ", "_"));
         --  Finally, we look for whole-component exceptions (such as
         --  "unsigned_short").
         if Case_Exceptions.Contains (S) then
            S := Case_Exceptions.Element (S);
         end if;
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

      --  Convert to space-separated.
      Result : String := Translate (Trim (Id, Both), To_Mapping ("_", " "));

      --  Find the dot-separated components
      Components : constant Spans := Find_Spans (Result, '.');

   begin
      for C in Components'Range loop
         Process_Component (Result (Components (C).L .. Components (C).U));
      end loop;
      return Result;
   end Normalize;


   function Abbreviate (Name : String) return String
   is
      Words : constant Spans := Find_Spans (Name, '_');
   begin
      if Words'Length = 1 then
         case Name (Name'First) is
            when 'A' | 'E' | 'I' | 'O' | 'U' => return "An_" & Name;
            when others => return "A_" & Name;
         end case;
      else
         declare
            Result : String (1 .. Words'Length);
         begin
            for J in Result'Range loop
               Result (J) := Name (Words (J - 1 + Words'First).L);
            end loop;
            return Result;
         end;
      end if;
   end Abbreviate;


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
