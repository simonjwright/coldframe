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
--  $Revision: 8da949757753 $
--  $Date: 2012/01/05 17:50:36 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Identifiers;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Attributes;
with Normalize_XMI.Model.Operations;

package body Normalize_XMI.Model.Classes is


   function Read_Class (From : DOM.Core.Node;
                        Parent : not null Element_P) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Class_Element;
      C : Class_Element renames Class_Element (N.all);
   begin
      C.Parent := Parent;
      C.Populate (From => From);
      C.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "... reading class " & (+C.Name));

      --  XXX will need a Tag eventually - how?
      C.Abbreviation := +Identifiers.Abbreviate (+C.Name);

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
               if C.Attributes.Contains (Name) then
                  Messages.Error
                    ("Class "
                       & (+C.Name)
                       & " has duplicate attribute "
                       & Name);
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
                 Operations.Read_Operation (DOM.Core.Nodes.Item (Nodes, J),
                                            Parent => N);
               Name : constant String := +O.Name;
            begin
               if C.Operations.Contains (Name) then
                  Messages.Error
                    ("Class "
                       & (+C.Name)
                       & " has duplicate operation "
                       & Name);
               else
                  C.Operations.Insert (Key => Name, New_Item => O);
               end if;
            end;
         end loop;
      end;

      return N;
   end Read_Class;


   not overriding
   procedure Create_Referential_Attribute
     (In_Class : in out Class_Element;
      Referring_To : Element_P;
      For_Relationship : Element_P;
      With_Source_Role_Name : String;
      Forming_Identifier : Boolean)
   is
      procedure Handle_If_Already_Formalized (Pos : Element_Maps.Cursor);
      Already_Formalized : Boolean := False;
      procedure Handle_If_Already_Formalized (Pos : Element_Maps.Cursor)
      is
         Formalizes : constant String
           := Element_Maps.Element (Pos).Tag_As_Name ("formalizes");
      begin
         if Formalizes = +For_Relationship.Name then
            Ada.Text_IO.Put_Line
              ("Association "
                 & (+For_Relationship.Name)
                 & " already formalized.");
            Already_Formalized := True;
         end if;
      end Handle_If_Already_Formalized;
   begin
      Element_Maps.Iterate (In_Class.Attributes,
                            Handle_If_Already_Formalized'Access);
      if not Already_Formalized then
         declare
            N : constant Element_P := new Referential_Attribute_Element;
            R : Referential_Attribute_Element
              renames Referential_Attribute_Element (N.all);
            use type Ada.Strings.Unbounded.Unbounded_String;
         begin
            R.Parent := In_Class'Unchecked_Access;
            R.Name :=
              For_Relationship.Name
              & (+".")
              & (+With_Source_Role_Name)
              & (+".")
              & Referring_To.Name;
            R.Referring_To := Referring_To;
            R.For_Relationship := For_Relationship;
            R.With_Source_Role_Name := +With_Source_Role_Name;
            R.Identifier := Forming_Identifier;
            In_Class.Attributes.Insert (Key => +R.Name, New_Item => N);
         end;
      end if;
   end Create_Referential_Attribute;


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
         if Visibility = "" or Visibility = "package" then
            Put (To, " visibility='private'");
         else
            Put (To, " visibility='" & Visibility & "'");
         end if;
      end;
      if C.Has_Stereotype ("active") then
         Put (To, " active='true'");
      end if;
      if C.Has_Stereotype ("public") then
         Put (To, " public='true'");
      end if;
      if C.Has_Stereotype ("singleton") then
         Put (To, " singleton='true'");
      end if;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+C.Name) & "</name>");
      Put_Line (To, "<abbreviation>" & (+C.Abbreviation) & "</abbreviation>");
      C.Output_Documentation (To);
      Element_Maps.Iterate (C.Attributes, Output'Access);
      Element_Maps.Iterate (C.Operations, Output'Access);
      Put_Line (To, "</class>");
   end Output;


   overriding
   procedure Output (R : Referential_Attribute_Element;
                     To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<attribute");
      Put (To, " refers='" & (+R.Referring_To.Name) & "'");
      Put (To, " relation='" & (+R.For_Relationship.Name) & "'");
      Put (To, " role='" & (+R.With_Source_Role_Name) & "'");
      if R.Identifier then
         Put (To, " identifier='true'");
      end if;
      Put_Line (To, "/>");
   end Output;


end Normalize_XMI.Model.Classes;
