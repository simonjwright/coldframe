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

--  $RCSfile: normalize_xmi-model-attributes.adb,v $
--  $Revision: 9a1e124a32ff $
--  $Date: 2011/12/18 19:08:23 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;

package body Normalize_XMI.Model.Attributes is


   function Read_Attribute (From : DOM.Core.Node) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Attribute_Element;
      A : Attribute_Element renames Attribute_Element (N.all);
   begin
      A.Populate (From => From);
      A.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "...... reading attribute " & (+A.Name));

      --  Type
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:StructuralFeature.type");
         pragma Assert
           (DOM.Core.Nodes.Length (Nodes) = 1,
            "should be 1 'UML:StructuralFeature.type' child of an Attribute");
      begin
         A.Type_Name := +Read_Name (DOM.Core.Nodes.Item (Nodes, 0));
      end;

      return N;
   end Read_Attribute;


   overriding
   procedure Resolve (A : in out Attribute_Element)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "...... checking attribute " & (+A.Name));
   end Resolve;


   overriding
   procedure Output (A : Attribute_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<attribute");
      --  if A.Has_Tag ("imported") then
      --     Put (To, " imported=""" & A.Tag_As_Name ("imported") & """");
      --  end if;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+A.Name) & "</name>");
      Put_Line (To, "<type>" & (+A.Type_Name) & "</type>");
      A.Output_Documentation (To);
      Put_Line (To, "</attribute>");
   end Output;


end Normalize_XMI.Model.Attributes;
