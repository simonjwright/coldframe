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

--  $RCSfile: normalize_xmi-model-operations.adb,v $
--  $Revision: 9809c9145d02 $
--  $Date: 2011/12/19 00:59:32 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Parameters;

package body Normalize_XMI.Model.Operations is


   function Read_Operation (From : DOM.Core.Node) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Operation_Element;
      O : Operation_Element renames Operation_Element (N.all);
   begin
      O.Populate (From => From);
      O.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "...... reading operation " & (+O.Name));

      --  Parameters
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From,
            "UML:BehavioralFeature.parameter/UML:Parameter"
              & "[not (@name='return')]");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               P : constant Element_P :=
                 Parameters.Read_Parameter (DOM.Core.Nodes.Item (Nodes, J));
               Name : constant String := +P.Name;
            begin
               P.Parent := O'Unchecked_Access;
               if O.Parameters.Contains (Name) then
                  Messages.Error
                    ("Operation "
                       & (+O.Name)
                       & " has duplicate parameter "
                       & Name);
               else
                  O.Parameters.Insert (Key => Name, New_Item => P);
               end if;
            end;
         end loop;
      end;

      --  Return
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From,
            "UML:BehavioralFeature.parameter/UML:Parameter"
              & "[(@name='return')]"
              & "/UML:Parameter.type/@name");
      begin
         if DOM.Core.Nodes.Length (Nodes) /= 0 then
            O.Return_Type :=
              +DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item (Nodes, 0));
         end if;
      end;

      return N;
   end Read_Operation;


   overriding
   procedure Resolve (O : in out Operation_Element)
   is
      use Ada.Text_IO;
      procedure Resolve (Pos : Element_Maps.Cursor);
      procedure Resolve (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve;
   begin
      Put_Line (Standard_Error, "...... checking operation " & (+O.Name));
      Element_Maps.Iterate (O.Parameters, Resolve'Access);
   end Resolve;


   overriding
   procedure Output (O : Operation_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      procedure Output (Pos : Element_Maps.Cursor);
      procedure Output (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Output (To);
      end Output;
   begin
      Put (To, "<operation");
      declare
         Is_Abstract : constant Boolean
           := Boolean'Value (Read_Attribute ("isAbstract",
                                             From_Element => O.Node));
      begin
         if Is_Abstract then
            Put (To, " abstract='true'");
         end if;
      end;
      declare
         Owner_Scope : constant String
           := Read_Attribute ("ownerScope", From_Element => O.Node);
      begin
         --  The other possibility is "instance".
         if Owner_Scope = "classifier" then
            Put (To, " class='true'");
         end if;
      end;
      if Ada.Strings.Unbounded.Length (O.Return_Type) > 0 then
         Put (To, " return='" & (+O.Return_Type) & "'");
      end if;
      declare
         Visibility : constant String
           := Read_Attribute ("visibility", From_Element => O.Node);
      begin
         if Visibility = "package" then
            Put (To, " visibility='public'");
         else
            Put (To, " visibility='" & Visibility & "'");
         end if;
      end;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+O.Name) & "</name>");
      O.Output_Documentation (To);
      Element_Maps.Iterate (O.Parameters, Output'Access);
      Put_Line (To, "</operation>");
   end Output;


end Normalize_XMI.Model.Operations;
