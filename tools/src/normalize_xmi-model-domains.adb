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

with Ada.Calendar;
with DOM.Core.Nodes;
with GNAT.Calendar.Time_IO;
with Normalize_XMI.Identifiers;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Association_Classes;
with Normalize_XMI.Model.Associations;
with Normalize_XMI.Model.Class_Types;
with Normalize_XMI.Model.Classes;
with Normalize_XMI.Model.Data_Types;
with Normalize_XMI.Model.Enumerations;
with Normalize_XMI.Model.Exceptions;
with Normalize_XMI.Model.Generalizations;
with XIA;

package body Normalize_XMI.Model.Domains is

   procedure Add_Standard_Types (To : in out Element_Maps.Map;
                                 In_Domain : Element_P);

   procedure Process_Domain (From    : not null DOM.Core.Node;
                             In_File : String)
   is
      D : aliased Domain (Parent => null);
   begin

      D.File_Time := GNAT.OS_Lib.File_Time_Stamp (In_File);

      --  Domain items
      --  Parent is, naturally, left null.
      D.Populate (From);

      --  Standard Types.
      Add_Standard_Types (To => D.Types, In_Domain => D'Unchecked_Access);

      --  Classes
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From,
            "descendant::UML:Class"
              & "[@xmi.id and not(UML:ModelElement.stereotype/UML:Stereotype/"
              & " @name='datatype')]");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               C : constant Element_P :=
                 Classes.Read_Class (DOM.Core.Nodes.Item (Nodes, J),
                                     Parent => D'Unchecked_Access);
            begin
               D.Classes.Insert (Key => +C.Name, New_Item => C);
            end;
         end loop;
      end;

      --  AssociationClasses
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "descendant::UML:AssociationClass[@xmi.id]");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               AC : constant Element_P :=
                 Association_Classes.Read_Association_Class
                 (DOM.Core.Nodes.Item (Nodes, J),
                  Parent => D'Unchecked_Access);
            begin
               D.Classes.Insert (Key => +AC.Name, New_Item => AC);
            end;
         end loop;
      end;

      --  ArgoUML allows a DataType to be given attributes; you just
      --  can't see them in the class diagram. From ColdFrame's point
      --  of view, it's much easier to implement statements about
      --  non-attributed types (such as 'null') using stereotypes
      --  (and, possibly, tagged values) on DataTypes only; so we
      --  handle class types and data types separately.

      --  Class <<datatype>>s
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From,
            "descendant::UML:Class"
              & "[@xmi.id and UML:ModelElement.stereotype/UML:Stereotype/"
              & " @name='datatype']");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               CT : constant Element_P :=
                 Class_Types.Read_Class_Type (DOM.Core.Nodes.Item (Nodes, J),
                                              Parent => D'Unchecked_Access);
            begin
               D.Types.Insert (Key => +CT.Name, New_Item => CT);
            end;
         end loop;
      end;

      --  DataTypes
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "descendant::UML:DataType[@xmi.id]");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               DT : constant Element_P :=
                 Data_Types.Read_Data_Type (DOM.Core.Nodes.Item (Nodes, J),
                                            Parent => D'Unchecked_Access);
            begin
               D.Types.Insert (Key => +DT.Name, New_Item => DT);
            end;
         end loop;
      end;

      --  Enumerations
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "descendant::UML:Enumeration[@xmi.id]");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               E : constant Element_P :=
                 Enumerations.Read_Enumeration (DOM.Core.Nodes.Item (Nodes, J),
                                                Parent => D'Unchecked_Access);
            begin
               D.Types.Insert (Key => +E.Name, New_Item => E);
            end;
         end loop;
      end;

      --  Associations
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "descendant::UML:Association[@xmi.id]");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               A : constant Element_P :=
                 Associations.Read_Association
                 (DOM.Core.Nodes.Item (Nodes, J),
                  Parent => D'Unchecked_Access);
            begin
               D.Associations.Insert (Key => +A.Name, New_Item => A);
            end;
         end loop;
      end;

      --  Generalizations
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "descendant::UML:Generalization[@xmi.id]");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            Generalizations.Read_Generalization
              (DOM.Core.Nodes.Item (Nodes, J),
               Parent => D'Unchecked_Access,
               Accumulating_In => D.Generalizations);
         end loop;
      end;

      --  Exceptions
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "descendant::UML:Exception[@xmi.id]");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               E : constant Element_P :=
                 Exceptions.Read_Exception (DOM.Core.Nodes.Item (Nodes, J),
                                            Parent => D'Unchecked_Access);
            begin
               D.Exceptions.Insert (Key => +E.Name, New_Item => E);
            end;
         end loop;
      end;

      if Messages.Number_Of_Errors /= 0 then
         --  Don't attempt to resolve the domain. This avoids nasty
         --  effects caused by incomplete population following errors
         --  during the read.
         return;
      end if;

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
   function Find_Class (Known_To        : Domain;
                        With_Model_Name : String) return Element_P
   is
   begin
      if Known_To.Classes.Contains (With_Model_Name) then
         return Known_To.Classes.Element (With_Model_Name);
      else
         return null;
      end if;
   end Find_Class;

   overriding
   function Find_Type (Known_To        : Domain;
                       With_Model_Name : String) return Element_P
   is
   begin
      if Known_To.Types.Contains (With_Model_Name) then
         return Known_To.Types.Element (With_Model_Name);
      else
         return null;
      end if;
   end Find_Type;

   overriding
   procedure Resolve (D : in out Domain)
   is
      procedure Resolve (Pos : Element_Maps.Cursor);
      procedure Resolve (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve;

      package String_Vectors is new Ada.Containers.Indefinite_Vectors
        (Index_Type   => Positive,
         Element_Type => String);
      package Abbreviation_Maps is new Ada.Containers.Indefinite_Ordered_Maps
        (Key_Type     => String,
         Element_Type => String_Vectors.Vector,
         "="          => String_Vectors."=");

      Abbreviations : Abbreviation_Maps.Map;

      procedure Collect_Class_Abbreviations (Pos : Element_Maps.Cursor);
      procedure Collect_Class_Abbreviations (Pos : Element_Maps.Cursor)
      is
         use Ada.Strings.Unbounded;
         Class_Name : constant String
           := To_String (Element_Maps.Element (Pos).Name);
         Maybe_Abbrev : constant String
           := Element_Maps.Element (Pos).Tag_Value ("abbreviation");
         Abbrev : constant String :=
           (if Maybe_Abbrev'Length /= 0
            then
               Maybe_Abbrev
            else
               Identifiers.Abbreviate (Class_Name));
      begin
         if not Abbreviations.Contains (Abbrev) then
            declare
               V : String_Vectors.Vector;
            begin
               Abbreviations.Insert (Abbrev, V);
            end;
         end if;
         Abbreviations (Abbrev).Append (Class_Name);
      end Collect_Class_Abbreviations;
   begin
      Messages.Information (" checking domain " & (+D.Name));
      D.Classes.Iterate (Resolve'Access);
      D.Classes.Iterate (Collect_Class_Abbreviations'Access);
      for Cursor in Abbreviations.Iterate loop
         if Natural (Abbreviation_Maps.Element (Cursor).Length) > 1 then
            Messages.Warning ("duplicate abbreviation """
                                & Abbreviation_Maps.Key (Cursor)
                                & """");
            for Name of Abbreviation_Maps.Element (Cursor) loop
               Messages.Information ("  ... class """ & Name & """");
            end loop;
         end if;
      end loop;
      D.Types.Iterate (Resolve'Access);
      D.Associations.Iterate (Resolve'Access);
      D.Generalizations.Iterate (Resolve'Access);
      D.Exceptions.Iterate (Resolve'Access);
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
      if D.Has_Tag ("name") then
         Put_Line (To, "<name>" & D.Tag_Value ("name") & "</name>");
      else
         Put_Line (To, "<name>" & (+D.Name) & "</name>");
      end if;
      Put_Line (To, "<extractor>normalize_xmi</extractor>");
      Output_Date (To);
      Put_Line (To, "<normalizer>normalize_xmi</normalizer>");
      D.Output_Documentation (To);
      if D.Has_Tag ("revision") then
         Put_Line (To,
                   "<revision>" & D.Tag_Value ("revision") & "</revision>");
      end if;
      if D.Has_Tag ("init") then
         Put_Line (To,
                   "<initialize>" & D.Tag_Value ("init") & "</initialize>");
      end if;

      D.Classes.Iterate (Output'Access);
      D.Types.Iterate (Output'Access);
      D.Associations.Iterate (Output'Access);
      D.Generalizations.Iterate (Output'Access);
      D.Exceptions.Iterate (Output'Access);

      Put_Line (To, "</domain>");
   end Output;

   procedure Add_Standard_Types (To : in out Element_Maps.Map;
                                 In_Domain : Element_P)
   is
      procedure Add_Standard_Type (Named : String);
      procedure Add_Standard_Type (Named : String)
      is
         N : constant Element_P := new Standard_Type_Element (In_Domain);
         ST : Standard_Type_Element renames Standard_Type_Element (N.all);
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

end Normalize_XMI.Model.Domains;
