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

--  $RCSfile: normalize_xmi-model-enumerations.adb,v $
--  $Revision: 5eca11a43724 $
--  $Date: 2011/12/14 18:22:37 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Errors;

package body Normalize_XMI.Model.Enumerations is


   function Read_Enumeration (From : DOM.Core.Node) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Enumeration_Element;
      E : Enumeration_Element renames Enumeration_Element (N.all);
   begin
      E.Populate (From => From);
      E.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "... reading enumeration " & (+E.Name));

      --  Literals
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:Enumeration.literal/UML:EnumerationLiteral");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            E.Literals.Append (Read_Name (DOM.Core.Nodes.Item (Nodes, J)));
         end loop;
      end;

      --  Operations

      return N;
   end Read_Enumeration;


   overriding
   procedure Resolve (E : in out Enumeration_Element)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "... checking enumeration " & (+E.Name));
   end Resolve;


   overriding
   procedure Output (E : Enumeration_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      procedure Output (Pos : String_Vectors.Cursor);
      procedure Output (Pos : String_Vectors.Cursor)
      is
      begin
         Put_Line (To,
                   "<literal>"
                     & String_Vectors.Element (Pos)
                     & "</literal>");
      end Output;
   begin
      Put_Line (To, "<type>");
      Put_Line (To, "<name>" & (+E.Name) & "</name>");
      E.Output_Documentation (To);
      Put_Line (To, "<enumeration>");
      String_Vectors.Iterate (E.Literals, Output'Access);
      Put_Line (To, "</enumeration>");
      Put_Line (To, "</type>");
   end Output;


end Normalize_XMI.Model.Enumerations;
