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
--  $Revision: 093f39d61362 $
--  $Date: 2011/12/14 21:26:48 $
--  $Author: simonjwright $

with Ada.Calendar;
with DOM.Core.Nodes;
with GNAT.Calendar.Time_IO;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Enumerations;
with Normalize_XMI.Model.Exceptions;
with Normalize_XMI.Model.Types;

package body Normalize_XMI.Model.Domains is

   procedure Add_Standard_Types (To : in out Element_Maps.Map);

   procedure Process_Domain (From : DOM.Core.Node; In_File : String)
   is
      D : aliased Domain;
   begin

      D.File_Time := GNAT.OS_Lib.File_Time_Stamp (In_File);

      --  Domain items
      D.Populate (From);
      D.Name := +Read_Name (From_Element => From);
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "... domain " & (+D.Name));

      --  Standard Types.
      Add_Standard_Types (To => D.Types);

      --  Types (including Enumerations).
      declare
         --  ArgoUML allows a DataType to be given attributes; you
         --  just can't see them in the class diagram. So there's
         --  really no difference from ColdFrame's point of
         --  view. Ideally we'd associate type-related tags with
         --  <<type>>, which looks silly if you already have
         --  <<datatype>>, so perhaps using a Class and stereotyping
         --  it <<type>> is neatest.
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From,
            "descendant::UML:Class[UML:ModelElement.stereotype/@name='type']"
              & "| descendant::UML:DataType");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               T : constant Element_P :=
                 Types.Read_Type (DOM.Core.Nodes.Item (Nodes, J));
            begin
               T.Parent := D'Unchecked_Access;
               D.Types.Insert (Key => +T.Name, New_Item => T);
            end;
         end loop;
      end;

      --  Enumerations
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "descendant::UML:Enumeration");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               E : constant Element_P :=
                 Enumerations.Read_Enumeration (DOM.Core.Nodes.Item (Nodes,
                                                                     J));
            begin
               E.Parent := D'Unchecked_Access;
               D.Types.Insert (Key => +E.Name, New_Item => E);
            end;
         end loop;
      end;

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

      if Messages.Number_Of_Errors = 0 then
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


   overriding
   procedure Resolve (D : in out Domain)
   is
      procedure Resolve (Pos : Element_Maps.Cursor);
      procedure Resolve (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve;
   begin
      Element_Maps.Iterate (D.Types, Resolve'Access);
      Element_Maps.Iterate (D.Exceptions, Resolve'Access);
   end Resolve;


   overriding
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
      D.Output_Documentation (To);
      Put_Line (To,
                "<revision>" & D.Tag_As_Value ("revision") & "</revision>");

      Element_Maps.Iterate (D.Types, Output'Access);
      Element_Maps.Iterate (D.Exceptions, Output'Access);

      Put_Line (To, "</domain>");
   end Output;


   type Standard_Type is new Element with null record;
   overriding
   procedure Resolve (ST : in out Standard_Type);
   overriding
   procedure Output (ST : Standard_Type; To : Ada.Text_IO.File_Type);


   procedure Add_Standard_Types (To : in out Element_Maps.Map)
   is
      procedure Add_Standard_Type (Named : String);
      procedure Add_Standard_Type (Named : String)
      is
         N : constant Element_P := new Standard_Type;
         ST : Standard_Type renames Standard_Type (N.all);
      begin
         ST.Name := +Named;
         To.Insert (Key => Named, New_Item => N);
      end Add_Standard_Type;
   begin
      Add_Standard_Type ("Autonumber");
      Add_Standard_Type ("Boolean");
      Add_Standard_Type ("Character");
      Add_Standard_Type ("Counterpart");
      Add_Standard_Type ("Date");
      Add_Standard_Type ("Duration");
      Add_Standard_Type ("Float");
      Add_Standard_Type ("Handle");
      Add_Standard_Type ("Integer");
      Add_Standard_Type ("Long_Float");
      Add_Standard_Type ("Natural");
      Add_Standard_Type ("Positive");
      Add_Standard_Type ("Real");
      Add_Standard_Type ("String");
      Add_Standard_Type ("Text");
      Add_Standard_Type ("Time");
      Add_Standard_Type ("Timer");
      Add_Standard_Type ("Unbounded_String");
   end Add_Standard_Types;


   overriding
   procedure Resolve (ST : in out Standard_Type)
   is
   begin
      null;
   end Resolve;


   overriding
   procedure Output (ST : Standard_Type; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put_Line (To, "<type standard='true'>");
      Put_Line (To, "<name>" & (+ST.Name) & "</name>");
      Put_Line (To, "</type>");
   end Output;


end Normalize_XMI.Model.Domains;
