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

--  $RCSfile: normalize_xmi-model-data_types.adb,v $
--  $Revision: 55c4c94ea007 $
--  $Date: 2012/01/23 00:29:31 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Operations;

package body Normalize_XMI.Model.Data_Types is


   function Read_Data_Type (From : DOM.Core.Node;
                            Parent : not null Element_P) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Data_Type_Element;
      T : Data_Type_Element renames Data_Type_Element (N.all);
   begin
      T.Parent := Parent;
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
   end Read_Data_Type;


   overriding
   procedure Resolve (T : in out Data_Type_Element)
   is
      use Ada.Text_IO;
      procedure Resolve (Pos : Element_Maps.Cursor);
      procedure Resolve (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve;
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
      if T.Has_Stereotype ("imported") and not T.Has_Tag ("imported") then
         Messages.Error
           ("Type "
              & (+T.Name)
              & " has <<imported>> but not {imported}.");
      end if;
      if T.Has_Stereotype ("renaming") and not T.Has_Tag ("renames") then
         Messages.Error
           ("Type "
              & (+T.Name)
              & " has <<renaming>> but not {renames}.");
      end if;
      T.Operations.Iterate (Resolve'Access);
   end Resolve;


   overriding
   procedure Output (T : Data_Type_Element; To : Ada.Text_IO.File_Type)
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
      if T.Has_Stereotype ("null") then
         Put (To, " null='true'");
      end if;
      if T.Has_Stereotype ("callback") then
         Put (To, " callback='true'");
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
      if T.Has_Tag ("imported") then
         Put_Line (To,
                   "<imported>" & T.Tag_As_Name ("imported") & "</imported>");
      end if;
      if T.Has_Tag ("renames") then
         Put_Line (To,
                   "<renames>" & T.Tag_As_Name ("renames") & "</renames>");
      end if;
      T.Operations.Iterate (Output'Access);
      Put_Line (To, "</type>");
   end Output;


end Normalize_XMI.Model.Data_Types;
