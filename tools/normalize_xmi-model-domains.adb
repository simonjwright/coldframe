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

--  $RCSfile: normalize_xmi-model-domains.adb,v $
--  $Revision: fd881b1b7e39 $
--  $Date: 2011/12/11 15:20:54 $
--  $Author: simonjwright $

with Ada.Calendar;
with DOM.Core.Nodes;
with GNAT.Calendar.Time_IO;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Errors;
with Normalize_XMI.Model.Exceptions;

package body Normalize_XMI.Model.Domains is

   procedure Process_Domain (From : DOM.Core.Node; In_File : String)
   is
      D : aliased Domain;
   begin

      D.File_Time := GNAT.OS_Lib.File_Time_Stamp (In_File);

      --  Domain items
      D.Name := +Read_Name (From_Element => From);
      D.Documentation
        := +Read_Tagged_Value ("documentation", From_Element => From);
      D.Revision
        := +Read_Tagged_Value ("revision", From_Element => From);

      --  Exceptions
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "descendant::UML:Exception");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               E : constant Element_P :=
                 Exceptions.Read_Exception (DOM.Core.Nodes.Item (Nodes, J));
            begin
               E.Parent := D'Unchecked_Access;
               D.Exceptions.Insert (Key => +E.Name, New_Item => E);
            end;
         end loop;
      end;

      D.Resolve;

      if Errors.Number_Of_Errors = 0 then
         declare
            use Ada.Text_IO;
            N : constant String := (+D.Name) & ".norm";
            O : Ada.Text_IO.File_Type;
         begin
            begin
               Open (O, Name => N, Mode => Out_File);
            exception
               when Name_Error =>
                  Create (O, Name => N, Mode => Out_File);
            end;
            D.Output (O);
            Close (O);
         end;
      end if;

   end Process_Domain;


   procedure Resolve (D : in out Domain)
   is
      procedure Resolve (Pos : Element_Maps.Cursor);
      procedure Resolve (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve;
   begin
      Element_Maps.Iterate (D.Exceptions, Resolve'Access);
   end Resolve;


   procedure Output (D : Domain; To : Ada.Text_IO.File_Type)
   is

      use Ada.Text_IO;

      procedure Output_Date (To : Ada.Text_IO.File_Type);
      procedure Output_Date (To : Ada.Text_IO.File_Type)
      is
         use Ada.Calendar;
         use GNAT.Calendar.Time_IO;
         use GNAT.OS_Lib;
         T : constant Time := Time_Of
           (Year => Year_Number (GM_Year (D.File_Time)),
            Month => Month_Number (GM_Month (D.File_Time)),
            Day => Day_Number (GM_Day (D.File_Time)),
            Seconds => Day_Duration (GM_Hour (D.File_Time) * 3600
                                       + GM_Minute (D.File_Time) * 60
                                       + GM_Second (D.File_Time)));
      begin
         Put_Line (To, Image (T,
                              "<date>"
                                & "<year>%Y</year>"
                                & "<month>%m</month>"
                                & "<day>%d</day>"
                                & "<time>%H:%M</time>"
                                & "</date>"));
      end Output_Date;

      procedure Output (Pos : Element_Maps.Cursor);
      procedure Output (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Output (To);
      end Output;

   begin
      Put_Line (To, "<domain>");
      Put_Line (To, "<name>" & (+D.Name) & "</name>");
      Put_Line (To, "<extractor>normalize_xmi</extractor>");
      Output_Date (To);
      Put_Line (To, "<normalizer>normalize_xmi</normalizer>");
      Output_Documentation (+D.Documentation, To);
      Put_Line (To, "<revision>" & (+D.Revision) & "</revision>");

      Element_Maps.Iterate (D.Exceptions, Output'Access);

      Put_Line (To, "</domain>");
   end Output;


end Normalize_XMI.Model.Domains;
