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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;

package body Normalize_XMI.Model.Data_Types is


   function Read_Data_Type (From : DOM.Core.Node) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Data_Type_Element;
      T : Data_Type_Element renames Data_Type_Element (N.all);
   begin
      T.Populate (From => From);
      T.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "... reading data type " & (+T.Name));

      --  Attributes
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:Classifier.feature/UML:Attribute");
      begin
         if DOM.Core.Nodes.Length (Nodes) > 0 then
            Messages.Error
              ("DataType "
              & (+T.Name)
              & " is not permitted to have attributes.");
         end if;
      end;

      --  Operations

      return N;
   end Read_Data_Type;


   overriding
   procedure Resolve (T : in out Data_Type_Element)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "... checking data type " & (+T.Name));
      --  There are all sorts of complicated illegal possibilities
      --  here!
      if T.Has_Tag ("imported") and T.Has_Tag ("renames") then
         Messages.Error
           ("Type "
              & (+T.Name)
              & " has both {imported} and {renames} specified.");
      end if;
   end Resolve;


   overriding
   procedure Output (T : Data_Type_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<type");
      if T.Has_Stereotype ("null") then
         Put (To, " null='true'");
      end if;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+T.Name) & "</name>");
      T.Output_Documentation (To);
      if T.Has_Tag ("imported") then
         Put_Line (To,
                   "<imported>" & T.Tag_As_Name ("imported") & "</imported>");
      end if;
      if T.Has_Tag ("renames") then
         Put_Line (To,
                   "<renames>" & T.Tag_As_Name ("renames") & "</renames>");
      end if;
      Put_Line (To, "</type>");
   end Output;


end Normalize_XMI.Model.Data_Types;
