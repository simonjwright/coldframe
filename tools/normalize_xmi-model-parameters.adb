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

--  $RCSfile: normalize_xmi-model-parameters.adb,v $
--  $Revision: 375f214b3bf4 $
--  $Date: 2011/12/18 22:23:02 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;

package body Normalize_XMI.Model.Parameters is


   function Read_Parameter (From : DOM.Core.Node) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Parameter_Element;
      P : Parameter_Element renames Parameter_Element (N.all);
   begin
      P.Populate (From => From);
      P.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "......... reading parameter " & (+P.Name));

      --  Type
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:Parameter.type");
         pragma Assert
           (DOM.Core.Nodes.Length (Nodes) = 1,
            "should be 1 'UML:Parameter.type' child of a Parameter");
      begin
         P.Type_Name := +Read_Name (DOM.Core.Nodes.Item (Nodes, 0));
      end;

      return N;
   end Read_Parameter;


   overriding
   procedure Resolve (P : in out Parameter_Element)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "......... checking parameter " & (+P.Name));
   end Resolve;


   overriding
   procedure Output (P : Parameter_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<parameter");
      declare
         Kind : constant String
           := Read_Attribute ("kind", From_Element => P.Node);
      begin
         Put (To, " mode='");
         if Kind = "in" then
            Put (To, "in");
         elsif Kind = "out" then
            Put (To, "out");
         elsif Kind = "inout" then
            Put (To, "inout");
         else
            Messages.Error ("unrecognised ""@kind='"
                              & Kind
                              & "'"" in parameter "
                              & (+P.Name));
         end if;
      end;
      Put_Line (To, "'>");
      Put_Line (To, "<name>" & (+P.Name) & "</name>");
      Put_Line (To, "<type>" & (+P.Type_Name) & "</type>");
      P.Output_Documentation (To);
      Put_Line (To, "</parameter>");
   end Output;


end Normalize_XMI.Model.Parameters;
