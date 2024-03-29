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

with DOM.Core.Nodes;
with Normalize_XMI.Model.Type_References;
with Normalize_XMI.Messages;
with XIA;

package body Normalize_XMI.Model.Attributes is

   function Read_Attribute (From   : not null DOM.Core.Node;
                            Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new Attribute_Element (Parent);
      A : Attribute_Element renames Attribute_Element (N.all);
   begin
      A.Populate (From => From);

      --  Type
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:StructuralFeature.type/*");
      begin
         if DOM.Core.Nodes.Length (Nodes) = 0 then
            Messages.Error ("No type specified for attribute "
                              & A.Fully_Qualified_Name);
         else
            A.Attribute_Type := Type_References.Read_Type_Reference
              (DOM.Core.Nodes.Item (Nodes, 0),
               Parent => N);
         end if;
      end;

      --  Initial value
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:Attribute.initialValue/UML:Expression");
      begin
         if DOM.Core.Nodes.Length (Nodes) > 0 then
            A.Initial_Value :=
              +Read_Attribute ("body",
                               From_Element => DOM.Core.Nodes.Item (Nodes, 0));
         end if;
      end;

      return N;
   end Read_Attribute;

   overriding
   procedure Resolve (A : in out Attribute_Element)
   is
   begin
      Messages.Trace ("...... checking attribute " & (+A.Name));
   end Resolve;

   overriding
   procedure Output (A : Attribute_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      T : Type_References.Type_Reference_Element
        renames Type_References.Type_Reference_Element (A.Attribute_Type.all);
   begin
      Put (To, "<attribute");
      if A.Has_Stereotype ("aliased") then
         Put (To, " aliased='true'");
      end if;
      if A.Has_Stereotype ("atomic") then
         Put (To, " atomic='true'");
      end if;
      declare
         Owner_Scope : constant String
           := Read_Attribute ("ownerScope", From_Element => A.Node);
      begin
         --  The other possibility is "instance".
         if Owner_Scope = "classifier" then
            Put (To, " class='true'");
         end if;
      end;
      if A.Has_Stereotype ("id") then
         Put (To, " identifier='true'");
         if A.Has_Tag ("formalizes") then
            Put (To, " refers='" & T.Type_Name & "'");
            Put (To, " relation='" & A.Tag_Value ("formalizes") & "'");
         end if;
      end if;
      if A.Has_Stereotype ("volatile") then
         Put (To, " volatile='true'");
      end if;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+A.Name) & "</name>");
      Put_Line (To, "<type>" & T.Type_Name & "</type>");
      if +A.Initial_Value /= "" then
         Put_Line (To, "<initial>" & (+A.Initial_Value) & "</initial>");
      end if;
      A.Output_Documentation (To);
      Put_Line (To, "</attribute>");
   end Output;

end Normalize_XMI.Model.Attributes;
