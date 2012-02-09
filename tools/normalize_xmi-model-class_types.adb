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

--  $RCSfile: normalize_xmi-model-class_types.adb,v $
--  $Revision: c3a01e5d21e1 $
--  $Date: 2012/02/09 17:17:31 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Attributes;
with Normalize_XMI.Model.Operations;

package body Normalize_XMI.Model.Class_Types is


   function Read_Class_Type (From   : not null DOM.Core.Node;
                             Parent : not null Element_P) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Class_Type_Element;
      T : Class_Type_Element renames Class_Type_Element (N.all);
   begin
      T.Parent := Parent;
      T.Populate (From => From);
      T.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "... reading class type " & (+T.Name));

      --  Attributes
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:Classifier.feature/UML:Attribute");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               A : constant Element_P :=
                 Attributes.Read_Attribute (DOM.Core.Nodes.Item (Nodes, J),
                                            Parent => N);
               Name : constant String := +A.Name;
            begin
               if T.Attributes.Contains (Name) then
                  Messages.Error
                    ("Type " & (+T.Name) & " has duplicate attribute " & Name);
               else
                  T.Attributes.Insert (Key => Name, New_Item => A);
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
                 Operations.Read_Operation (DOM.Core.Nodes.Item (Nodes, J),
                                            Parent => N);
               Name : constant String := +O.Name;
            begin
               if T.Operations.Contains (Name) then
                  Messages.Error
                    ("Type " & (+T.Name) & " has duplicate operation " & Name);
               else
                  T.Operations.Insert (Key => Name, New_Item => O);
               end if;
            end;
         end loop;
      end;

      return N;
   end Read_Class_Type;


   overriding
   procedure Resolve (T : in out Class_Type_Element)
   is
      use Ada.Text_IO;
      procedure Resolve (Pos : Element_Maps.Cursor);
      procedure Resolve (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve;
   begin
      Put_Line (Standard_Error, "... checking class type " & (+T.Name));
      if T.Attributes.Is_Empty then
         Messages.Warning
           ("Type "
              & (+T.Name)
              & " has no attributes, assumed null.");
      end if;
      T.Attributes.Iterate (Resolve'Access);
      T.Operations.Iterate (Resolve'Access);
   end Resolve;


   overriding
   procedure Output (T : Class_Type_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      procedure Output (Pos : Element_Maps.Cursor);
      procedure Output (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Output (To);
      end Output;
   begin
      Put (To, "<type");
      if T.Accessor /= null then
         Put (To, " access='" & (+T.Accessor.Name) & "'");
      end if;
      if T.Has_Stereotype ("callback") then
         Put (To, " callback='true'");
      end if;
      if T.Attributes.Is_Empty then
         Put (To, " null='true'");
      end if;
      declare
         Visibility : constant String
           := Read_Attribute ("visibility", From_Element => T.Node);
      begin
         if Visibility = "" or Visibility = "package" then
            Put (To, " visibility='private'");
         else
            Put (To, " visibility='" & Visibility & "'");
         end if;
      end;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+T.Name) & "</name>");
      T.Output_Documentation (To);
      T.Attributes.Iterate (Output'Access);
      T.Operations.Iterate (Output'Access);
      Put_Line (To, "</type>");
   end Output;


end Normalize_XMI.Model.Class_Types;
