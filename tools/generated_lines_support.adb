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

--  $RCSfile: generated_lines_support.adb,v $
--  $Revision: 76f35906ecb5 $
--  $Date: 2013/05/15 16:17:11 $
--  $Author: simonjwright $

with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regpat; use GNAT.Regpat;

package body Generated_Lines_Support is

   type Pattern is access constant Pattern_Matcher;
   type Name is access constant String;

   type Info_Base (Named : Name; For_Pattern : Pattern) is record
      Files : Natural := 0;
      Lines : Natural := 0;
   end record;
   type Info is access Info_Base;

   package Collections is new Ada.Containers.Vectors (Positive, Info);

   Patterns : Collections.Vector;

   procedure Setup (Pattern, Named : String);
   function Count_Lines (In_File_Named : Path_Name) return Natural;


   procedure Count (File_Named : Path_Name;
                    Verbosely : Boolean;
                    Logging : Boolean) is
      N : constant Path_Name := File_Name (File_Named);
      It : Collections.Cursor := Patterns.First;
      use Collections;
   begin
      loop
         if It = No_Element then
            if Verbosely then
               Put_Line (Standard_Error,
                         "file " & File_Named & " didn't match any rule.");
            end if;
            exit;
         end if;
         if Match (Element (It).For_Pattern.all, N) >= N'First then
            declare
               I : Info renames Element (It);
            begin
               if Logging then
                  Put_Line (Standard_Error, I.Named.all & " caught " & N);
               end if;
               I.Files := I.Files + 1;
               I.Lines := I.Lines + Count_Lines (File_Named);
            end;
            exit;
         end if;
         Next (It);
      end loop;
   end Count;


   procedure Report (For_Directory : Path_Name;
                     With_Header : Boolean) is
      It : Collections.Cursor := Patterns.First;
      use Collections;
   begin
      if With_Header then
         Put_Line ("Directory, Category, Files, Lines,");
      end if;
      while It /= No_Element loop
         declare
            I : Info renames Element (It);
         begin
            Put_Line (Base_Name (For_Directory) & ","
                        & I.Named.all & ","
                        & I.Files'Img & ","
                        & I.Lines'Img & ",");
         end;
         Next (It);
      end loop;
   end Report;


   procedure Setup (Pattern, Named : String) is
   begin
      Collections.Append
        (Patterns,
         new Info_Base'
           (Named => new String'(Named),
            For_Pattern => new Pattern_Matcher'(Compile (Pattern & "$")),
            Files => 0,
            Lines => 0));
   end Setup;


   Comment_Matcher : constant Pattern_Matcher := Compile ("(--.*)$");

   function Count_Lines (In_File_Named : Path_Name) return Natural is
      F : File_Type;
      Result : Natural := 0;
      Line : String (1 .. 1024);
      Last : Natural;
      Matches : Match_Array (0 .. 1);
   begin
      Open (F, Mode => In_File, Name => In_File_Named);
      loop
         Get_Line (F, Line, Last);
         Match (Comment_Matcher, Line (1 .. Last), Matches);
         if Matches (0) /= No_Match then
            Line (Matches (1).First .. Matches (1).Last) := (others => ' ');
         end if;
         for C in 1 .. Last loop
            if Line (C) = ';' then
               Result := Result + 1;
            end if;
         end loop;
         exit when End_Of_File (F);
      end loop;
      Close (F);
      return Result;
   end Count_Lines;


begin

   Setup ("initialize\.ad[bs]", "initialization");
   Setup ("tear_down\.ad[bs]", "test");
   Setup ("^[^-]*-events(-initialize)?.ad[bs]", "events");
   Setup ("-inheritance\.ad[bs]", "advanced inheritance");
   Setup ("-(filter|selection)_function\.ad[bs]", "selection");
   Setup ("-iterate\.ad[bs]", "iteration");
   Setup ("-(handle|instance)_hash\.ad[bs]", "hashing");
   Setup ("-(abstract_)?(containers|collections).ad[bs]", "containers");
   Setup ("-(abstract_)?sets.ad[bs]", "advanced containers");
   Setup ("-from_collections\.ad[bs]", "navigation from collections");
   Setup ("-all_instances\.ad[bs]", "all instances");
   Setup ("-serializable\.ad[bs]", "serialization");

   Setup ("^.*-.*-t\.adb", "tasks");
   Setup ("^.*-.*-.*\.adb", "operations");

   Setup ("\.ad[bs]", "classes/associations");

end Generated_Lines_Support;
