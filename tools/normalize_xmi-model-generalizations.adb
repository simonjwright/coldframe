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

--  $RCSfile: normalize_xmi-model-generalizations.adb,v $
--  $Revision: f9be220a35c7 $
--  $Date: 2014/01/02 20:18:20 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Identifiers;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Classes;

package body Normalize_XMI.Model.Generalizations is


   procedure Read_Generalization
     (From            :        not null DOM.Core.Node;
      Parent          :        not null Element_P;
      Accumulating_In : in out Element_Maps.Map)
   is
      Model_Name : constant String :=
        Read_Attribute ("name", From_Element => From);
      Discriminator : constant String :=
        Read_Attribute ("discriminator", From_Element => From);
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Parent_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query (From,
                                            "UML:Generalization.parent/*");
      Parent_Name : constant String
        := Read_Attribute
          ("name", From_Element => DOM.Core.Nodes.Item (Parent_Nodes, 0));
      Child_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query (From,
                                            "UML:Generalization.child/*");
      Child_Name : constant String
        := Read_Attribute
          ("name", From_Element => DOM.Core.Nodes.Item (Child_Nodes, 0));
      N : Element_P;
   begin
      if Model_Name'Length = 0 then
         if Discriminator'Length = 0 then
            Messages.Error ("Unnamed/undiscriminated generalization from "
                              & Child_Name
                              & " to "
                              & Parent_Name);
            return;
         else
            if Identifiers.Is_Valid (Discriminator) then
               Name := +Identifiers.Normalize (Discriminator);
            else
               Messages.Error ("Invalid discriminator """
                                 & Discriminator
                                 & """ in generalization from "
                                 & Child_Name
                                 & " to "
                                 & Parent_Name);
               Name := +Discriminator;
            end if;
         end if;
      else
         if Discriminator'Length = 0 or else Discriminator = Model_Name then
            if Identifiers.Is_Valid (Model_Name) then
               Name := +Identifiers.Normalize (Model_Name);
            else
               Name := +Model_Name;
               Messages.Error ("Invalid name """
                                 & Model_Name
                                 & """ in generalization from "
                                 & Child_Name
                                 & " to "
                                 & Parent_Name);
            end if;
         else
            Messages.Error ("Mismatch between generalization name ("
                              & Model_Name
                              & ") and discriminator ("
                              & Discriminator
                              & ") from "
                              & Child_Name
                              & " to "
                              & Parent_Name);
            return;
         end if;
      end if;
      if Accumulating_In.Contains (+Name) then
         N := Accumulating_In.Element (+Name);
      else
         N := new Generalization_Element (Parent);
         Accumulating_In.Insert (Key => +Name, New_Item => N);
         N.Populate (From); -- An invalid model name will be reported again
         N.Name := Name;    -- Override the default chosen by Populate
         if Identifiers.Is_Valid (Parent_Name) then
            Generalization_Element (N.all).Parent_Class
              := N.Find_Class (Parent_Name);
         else
            --  Unless the .uml is badly broken, the invalidity will
            --  be reported when the parent class is
            --  processed. However, we can't go any further, because
            --  the declare block below would raise CE.
            return;
         end if;
      end if;
      declare
         G : Generalization_Element renames Generalization_Element (N.all);
      begin
         if +G.Parent_Class.Name /= Parent_Name then
            Messages.Error ("Generalization "
                              & (+Name)
                              & " from "
                              & Child_Name
                              & " has different parent ("
                              & Parent_Name
                              & ") from previously found");
            return;
         end if;
         if Identifiers.Is_Valid (Child_Name) then
            G.Child_Classes.Append
              (G.Find_Class (Identifiers.Normalize (Child_Name)));
         else
            --  Unless the .uml is badly broken, the invalidity will
            --  be reported when the child class is
            --  processed.
            return;
         end if;
      end;
   end Read_Generalization;


   overriding
   procedure Resolve (G : in out Generalization_Element)
   is
      procedure Resolve (Pos : Element_Vectors.Cursor);
      procedure Resolve (Pos : Element_Vectors.Cursor)
      is
         Child : Classes.Class_Element
           renames Classes.Class_Element (Element_Vectors.Element (Pos).all);
      begin
         Child.Create_Referential_Attribute
           (Referring_To => G.Parent_Class,
            For_Relationship => G'Unchecked_Access,
            With_Source_Role_Name => "Parent",
            Forming_Identifier => True);
      end Resolve;
   begin
      Messages.Trace ("... checking generalization " & (+G.Name));
      G.Child_Classes.Iterate (Resolve'Access);
   end Resolve;


   overriding
   procedure Output (G : Generalization_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      procedure Output (Pos : Element_Vectors.Cursor);
      procedure Output (Pos : Element_Vectors.Cursor)
      is
      begin
         Put_Line
           (To,
            "<child>" & (+Element_Vectors.Element (Pos).Name) & "</child>");
      end Output;
   begin
      Put (To, "<inheritance");
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+G.Name) & "</name>");
      Put_Line (To, "<parent>" & (+G.Parent_Class.Name) & "</parent>");
      G.Child_Classes.Iterate (Output'Access);
      G.Output_Documentation (To);
      Put_Line (To, "</inheritance>");
   end Output;


end Normalize_XMI.Model.Generalizations;
