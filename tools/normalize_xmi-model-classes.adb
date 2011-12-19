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

--  $RCSfile: normalize_xmi-model-classes.adb,v $
--  $Revision: 980cab1dde3f $
--  $Date: 2011/12/19 14:37:26 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Attributes;
with Normalize_XMI.Model.Operations;

package body Normalize_XMI.Model.Classes is


   function Read_Class (From : DOM.Core.Node) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Class_Element;
      C : Class_Element renames Class_Element (N.all);
   begin
      C.Populate (From => From);
      C.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "... reading class " & (+C.Name));

      --  Attributes
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:Classifier.feature/UML:Attribute");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               A : constant Element_P :=
                 Attributes.Read_Attribute (DOM.Core.Nodes.Item (Nodes, J));
               Name : constant String := +A.Name;
            begin
               A.Parent := C'Unchecked_Access;
               if C.Attributes.Contains (Name) then
                  Messages.Error
                    ("Type " & (+C.Name) & " has duplicate attribute " & Name);
               else
                  C.Attributes.Insert (Key => Name, New_Item => A);
               end if;
            end;
         end loop;
      end;

      --  Operations
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:Classifier.feature/UML:Operation");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               O : constant Element_P :=
                 Operations.Read_Operation (DOM.Core.Nodes.Item (Nodes, J));
               Name : constant String := +O.Name;
            begin
               O.Parent := C'Unchecked_Access;
               if C.Operations.Contains (Name) then
                  Messages.Error
                    ("Type " & (+C.Name) & " has duplicate operation " & Name);
               else
                  C.Operations.Insert (Key => Name, New_Item => O);
               end if;
            end;
         end loop;
      end;

      return N;
   end Read_Class;


   overriding
   procedure Resolve (C : in out Class_Element)
   is
      use Ada.Text_IO;
      procedure Resolve (Pos : Element_Maps.Cursor);
      procedure Resolve (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve;
   begin
      Put_Line (Standard_Error, "... checking class " & (+C.Name));
      Element_Maps.Iterate (C.Attributes, Resolve'Access);
      Element_Maps.Iterate (C.Operations, Resolve'Access);
   end Resolve;


   overriding
   procedure Output (C : Class_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      procedure Output (Pos : Element_Maps.Cursor);
      procedure Output (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Output (To);
      end Output;
   begin
      Put (To, "<class");
      declare
         Visibility : constant String
           := Read_Attribute ("visibility", From_Element => C.Node);
      begin
         if Visibility = "package" then
            Put (To, " visibility='private'");
         else
            Put (To, " visibility='" & Visibility & "'");
         end if;
      end;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+C.Name) & "</name>");
      C.Output_Documentation (To);
      Element_Maps.Iterate (C.Attributes, Output'Access);
      Element_Maps.Iterate (C.Operations, Output'Access);
      Put_Line (To, "</class>");
   end Output;


end Normalize_XMI.Model.Classes;
