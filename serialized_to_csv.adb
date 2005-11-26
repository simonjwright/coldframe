--  Copyright (c) Simon Wright <simon@pushface.org>

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

--  Provides conversion from the XML output of ColdFrams's
--  'serialization' facility to CSV files.

--  $Id: serialized_to_csv.adb,v e6c41bb0a7c7 2005/11/26 16:20:27 simonjwright $

with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
with Serialized_To_Csv_Support;

procedure Serialized_To_Csv is

   use Ada.Text_IO;
   use Ada.Strings;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Unbounded.Text_IO;
   use Serialized_To_Csv_Support;
   use Serialized_To_Csv_Support.Access_File_Maps;

   function "+" (S : String) return Unbounded_String
     renames To_Unbounded_String;
   function "+" (S : Unbounded_String) return String
     renames To_String;

   procedure Escape_Value (V : in out Unbounded_String);
   procedure Process_Record (Output : File_Type; New_File : Boolean);

   procedure Escape_Value (V : in out Unbounded_String) is
   begin
      null;
   end Escape_Value;

   procedure Process_Record (Output : File_Type; New_File : Boolean) is
      Line : Unbounded_String;
      Field_Names : Unbounded_String;
      Values : Unbounded_String;
   begin
      loop
         Line := Get_Line;
         exit when Index (Line, "</record>") /= 0;
         if Index (Line, "<field name") /= 0 then
            if New_File then
               Append (Field_Names,
                       +(Slice (Line,
                                Index (Line, """") + 1,
                                Index (Line, """", Going => Backward) - 1)
                           & ','));
            end if;
            declare
               Value : Unbounded_String
                 := +Slice (Line,
                            Index (Line, ">") + 1,
                            Index (Line, "<", Going => Backward) - 1);
            begin
               Escape_Value (Value);
               Append (Values, Value);
               Append (Values, ",");
            end;
         end if;
      end loop;
      if New_File then
         Put_Line (Output, Field_Names);
      end if;
      Put_Line (Output, Values);
      --  Make sure the output is written even if we're stopped by ^C
      Flush (Output);
   end Process_Record;

   Files : Map;
   Line : Unbounded_String;
   Records : Natural := 0;

begin

   while not End_Of_File loop

      Line := Get_Line;

      if Index (Line, "<record name") > 0 then

         declare
            Record_Name :  constant Unbounded_String
              := +Slice (Line,
                         Index (Line, """") + 1,
                         Index (Line, """", Going => Backward) - 1);
            Output : Access_File;
         begin
            if Is_Bound (Files, Record_Name) then
               Output := Item_Of (Files, Record_Name);
               Process_Record (Output.all, New_File => False);
            else
               Put_Line (Record_Name);
               Output := new File_Type;
               begin
                  Open (Output.all,
                        Name => +Record_Name & ".csv",
                        Mode => Out_File);
               exception
                  when Name_Error =>
                     Create (Output.all,
                             Name => +Record_Name & ".csv",
                             Mode => Out_File);
               end;
               Bind (Files, Record_Name, Output);
               Process_Record (Output.all, New_File => True);
            end if;
         end;

         --  Progress marker
         Records := Records + 1;
         if Records mod 64 = 0 then
            Put (Standard_Error, '.');
         end if;

      end if;

   end loop;

end Serialized_To_Csv;

