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
--  $Revision: 28f010f797e0 $
--  $Date: 2013/07/14 17:12:36 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Operations;

package body Normalize_XMI.Model.Enumerations is


   function Read_Enumeration (From   : not null DOM.Core.Node;
                              Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new Enumeration_Element;
      E : Enumeration_Element renames Enumeration_Element (N.all);
   begin
      E.Parent := Parent;
      E.Populate (From => From);
      E.Name := +Read_Name (From_Element => From);
      Messages.Trace ("... reading enumeration " & (+E.Name));

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
               if E.Operations.Contains (Name) then
                  Messages.Error
                    ("Type " & (+E.Name) & " has duplicate operation " & Name);
               else
                  E.Operations.Insert (Key => Name, New_Item => O);
               end if;
            end;
         end loop;
      end;

      return N;
   end Read_Enumeration;


   overriding
   procedure Resolve (E : in out Enumeration_Element)
   is
      procedure Resolve (Pos : Element_Maps.Cursor);
      procedure Resolve (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve;
   begin
      Messages.Trace ("... checking enumeration " & (+E.Name));
      if E.Has_Stereotype ("convention")
        and not E.Has_Tag ("language") then
         Messages.Error
           ("Type "
              & (+E.Name)
              & " has <<convention>> but not {language}.");
      end if;
      E.Operations.Iterate (Resolve'Access);
   end Resolve;


   overriding
   procedure Output (E : Enumeration_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      --  I've named these Outputs differently because GNAT (both GCC
      --  4.6 and GNAT GPL 2011) thinks there's an ambiguity.
      procedure Output_Operation (Pos : Element_Maps.Cursor);
      procedure Output_Operation (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Output (To);
      end Output_Operation;
      procedure Output_Literal (Pos : String_Vectors.Cursor);
      procedure Output_Literal (Pos : String_Vectors.Cursor)
      is
      begin
         Put_Line (To,
                   "<literal>"
                     & String_Vectors.Element (Pos)
                     & "</literal>");
      end Output_Literal;
   begin
      Put (To, "<type");
      if E.Accessor /= null then
         Put (To, " access='" & (+E.Accessor.Name) & "'");
      end if;
      if E.Has_Stereotype ("callback") then
         Put (To, " callback='true'");
      end if;
      if E.Has_Stereotype ("convention") then
         Put (To, " convention='" & E.Tag_As_Name ("language") & "'");
      end if;
      declare
         Visibility : constant String
           := Read_Attribute ("visibility", From_Element => E.Node);
      begin
         if Visibility = "" or Visibility = "package" then
            Put (To, " visibility='private'");
         else
            Put (To, " visibility='" & Visibility & "'");
         end if;
      end;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+E.Name) & "</name>");
      E.Output_Documentation (To);
      Put_Line (To, "<enumeration>");
      E.Literals.Iterate (Output_Literal'Access);
      Put_Line (To, "</enumeration>");
      E.Operations.Iterate (Output_Operation'Access);
      Put_Line (To, "</type>");
   end Output;


end Normalize_XMI.Model.Enumerations;
