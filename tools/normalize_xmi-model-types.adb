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

--  $RCSfile: normalize_xmi-model-types.adb,v $
--  $Revision: 093f39d61362 $
--  $Date: 2011/12/14 21:26:48 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Model.Attributes;
with Normalize_XMI.Messages;

package body Normalize_XMI.Model.Types is


   function Read_Type (From : DOM.Core.Node) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Type_Element;
      T : Type_Element renames Type_Element (N.all);
   begin
      T.Populate (From => From);
      T.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "... reading type " & (+T.Name));

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
               A.Parent := T'Unchecked_Access;
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

      return N;
   end Read_Type;


   overriding
   procedure Resolve (T : in out Type_Element)
   is
      use Ada.Text_IO;
      procedure Resolve (Pos : Element_Maps.Cursor);
      procedure Resolve (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve;
   begin
      Put_Line (Standard_Error, "... checking type " & (+T.Name));
      --  There are all sorts of complicated illegal possibilities
      --  here!
      T.Attributes_Permitted :=
        not (T.Has_Tag ("imported")
               or T.Has_Tag ("renames"));
      if T.Has_Tag ("imported") and T.Has_Tag ("renames") then
         Messages.Error
           ("Type "
              & (+T.Name)
              & " has both {imported} and {renames} specified.");
      end if;
      if T.Attributes_Permitted then
         if T.Attributes.Is_Empty then
            Messages.Warning
              ("Type "
              & (+T.Name)
              & " has no attributes, assumed null.");
         end if;
      else
         if not T.Attributes.Is_Empty then
            Messages.Error
              ("Type "
              & (+T.Name)
              & " is not permitted to have attributes.");
         end if;
      end if;
      Element_Maps.Iterate (T.Attributes, Resolve'Access);
   end Resolve;


   overriding
   procedure Output (T : Type_Element; To : Ada.Text_IO.File_Type)
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
      if T.Attributes_Permitted and T.Attributes.Is_Empty then
         Put (To, " null='true'");
      end if;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+T.Name) & "</name>");
      T.Output_Documentation (To);
      if T.Has_Tag ("imported") then
         Put_Line (To,
                   "<imported>" & T.Tag_As_Name ("imported") & "</imported>");
      elsif T.Has_Tag ("renames") then
         Put_Line (To,
                   "<renames>" & T.Tag_As_Name ("renames") & "</renames>");
      else
         Element_Maps.Iterate (T.Attributes, Output'Access);
      end if;
      Put_Line (To, "</type>");
   end Output;


end Normalize_XMI.Model.Types;
