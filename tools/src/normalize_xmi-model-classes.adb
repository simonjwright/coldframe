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
with Normalize_XMI.Identifiers;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Attributes;
with Normalize_XMI.Model.Operations;
with Normalize_XMI.Model.State_Machines;
with XIA;

package body Normalize_XMI.Model.Classes is

   function Read_Class (From   : not null DOM.Core.Node;
                        Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new Class_Element (Parent);
   begin
      Populate_Class_Aspects (N, From);
      return N;
   end Read_Class;

   procedure Populate_Class_Aspects
     (Of_Element : not null Element_P;
      From       : not null DOM.Core.Node) is
      C : Class_Element renames Class_Element (Of_Element.all);
   begin
      C.Populate (From => From);

      --  Attributes
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:Classifier.feature/UML:Attribute");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               A : constant Element_P :=
                 Attributes.Read_Attribute (DOM.Core.Nodes.Item (Nodes, J),
                                            Parent => Of_Element);
               Name : constant String := +A.Name;
            begin
               if C.Attributes.Contains (Name) then
                  Messages.Error
                    ("Class "
                       & C.Fully_Qualified_Name
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
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:Classifier.feature/UML:Operation");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               O : constant Element_P :=
                 Operations.Read_Operation (DOM.Core.Nodes.Item (Nodes, J),
                                            Parent => Of_Element);
            begin
               C.Operations.Append (O);
            end;
         end loop;
      end;

      --  State Machines (a): class
      declare
         Xmi_Id : constant String
           := Read_Attribute ("xmi.id", From_Element => From);
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "../UML:StateMachine"
              & "[UML:StateMachine.context/UML:Class/@xmi.idref='"
              & Xmi_Id
              & "' and UML:ModelElement.stereotype/UML:Stereotype/"
              & "      @name='class']");
      begin
         case DOM.Core.Nodes.Length (Nodes) is
            when 0 => null;
            when 1 =>
               C.State_Machines.Append
                 (State_Machines.Read_State_Machine
                    (DOM.Core.Nodes.Item (Nodes, 0),
                     Parent => Of_Element));
            when others =>
               Messages.Error
                 ("Class "
                    & C.Fully_Qualified_Name
                    & " has more than one <<class>> state machine");
         end case;
      end;

      --  State Machines (b): instance
      declare
         Xmi_Id : constant String
           := Read_Attribute ("xmi.id", From_Element => From);
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "../UML:StateMachine"
              & "[UML:StateMachine.context/UML:Class/@xmi.idref='"
              & Xmi_Id
              & "' and not(UML:ModelElement.stereotype/UML:Stereotype/"
              & "          @name='class')]");
      begin
         case DOM.Core.Nodes.Length (Nodes) is
            when 0 => null;
            when 1 =>
               C.State_Machines.Append
                 (State_Machines.Read_State_Machine
                    (DOM.Core.Nodes.Item (Nodes, 0),
                     Parent => Of_Element));
            when others =>
               Messages.Error
                 ("Class "
                    & C.Fully_Qualified_Name
                    & " has more than one instance state machine");
         end case;
      end;
   end Populate_Class_Aspects;

   procedure Output_Class_Aspects (C  : Class_Element'Class;
                                   To : Ada.Text_IO.File_Type) is
      use Ada.Text_IO;
      procedure Output_M (Pos : Element_Maps.Cursor);
      procedure Output_V (Pos : Element_Vectors.Cursor);
      procedure Output_M (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Output (To);
      end Output_M;
      procedure Output_V (Pos : Element_Vectors.Cursor)
      is
      begin
         Element_Vectors.Element (Pos).Output (To);
      end Output_V;
   begin
      declare
         Visibility : constant String
           := Read_Attribute ("visibility", From_Element => C.Node);
      begin
         if Visibility = "" or else Visibility = "package" then
            Put (To, " visibility='private'");
         else
            Put (To, " visibility='" & Visibility & "'");
         end if;
      end;
      if C.Has_Stereotype ("active")
        or else Boolean'Value (Read_Attribute ("isActive",
                                               From_Element => C.Node))
      then
         Put (To, " active='true'");
         if C.Has_Tag ("priority") then
            Put (To, " priority='" & C.Tag_Value ("priority") & "'");
         end if;
         if C.Has_Tag ("stack") then
            Put (To, " stack='" & C.Tag_Value ("stack") & "'");
         end if;
      end if;
      if C.Has_Stereotype ("cardinality") then
         if C.Has_Tag ("max") then
            Put (To, " max='" & C.Tag_Value ("max") & "'");
         else
            Messages.Error
              ("Class "
                 & C.Fully_Qualified_Name
                 & " has <<cardinality>> but not {max}");
         end if;
      end if;
      if C.Has_Stereotype ("public") then
         Put (To, " public='true'");
      end if;
      if C.Has_Stereotype ("singleton") then
         Put (To, " singleton='true'");
      end if;
      if C.Has_Stereotype ("utility") then
         declare
            use type Ada.Containers.Count_Type;
         begin
            if C.Attributes.Length > 0 then
               Messages.Error
                 ("Utility class "
                    & C.Fully_Qualified_Name
                    & " has attributes");
            end if;
            if C.Operations.Length = 0 then
               Messages.Warning
                 ("Utility class "
                    & C.Fully_Qualified_Name
                    & " has no operations");
            end if;
         end;
         Put (To, " utility='true'");
      end if;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+C.Name) & "</name>");
      declare
         Abbrev : constant String
           := (if C.Has_Tag ("abbreviation")
               then C.Tag_Value ("abbreviation")
               else Identifiers.Abbreviate (+C.Name));
      begin
         if not Identifiers.Is_Valid (Abbrev) then
            Messages.Warning ("invalid abbreviation """
                                & Abbrev
                                & """ for class """
                                & (+C.Name) & """");
         end if;
         Put_Line (To, "<abbreviation>" & Abbrev & "</abbreviation>");
      end;
      C.Output_Documentation (To);
      C.Attributes.Iterate (Output_M'Access);
      C.Operations.Iterate (Output_V'Access);
      C.State_Machines.Iterate (Output_V'Access);
   end Output_Class_Aspects;

   not overriding
   procedure Create_Referential_Attribute
     (In_Class              : in out Class_Element;
      Referring_To          :        not null Element_P;
      For_Relationship      :        not null Element_P;
      With_Source_Role_Name :        String;
      Forming_Identifier    :        Boolean)
   is
      procedure Handle_If_Already_Formalized (Pos : Element_Maps.Cursor);
      Already_Formalized : Boolean := False;
      procedure Handle_If_Already_Formalized (Pos : Element_Maps.Cursor)
      is
         Formalizes : constant String
           := Element_Maps.Element (Pos).Tag_Value ("formalizes");
      begin
         if Formalizes = +For_Relationship.Name then
            Messages.Information
              ("Association "
                 & (+For_Relationship.Name)
                 & " already formalized");
            Already_Formalized := True;
         end if;
      end Handle_If_Already_Formalized;
   begin
      In_Class.Attributes.Iterate (Handle_If_Already_Formalized'Access);
      if not Already_Formalized then
         declare
            N : constant Element_P := new Referential_Attribute_Element
              (Parent => In_Class'Unchecked_Access);
            R : Referential_Attribute_Element
              renames Referential_Attribute_Element (N.all);
            use type Ada.Strings.Unbounded.Unbounded_String;
         begin
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
      procedure Resolve_M (Pos : Element_Maps.Cursor);
      procedure Resolve_V (Pos : Element_Vectors.Cursor);
      procedure Resolve_M (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve_M;
      procedure Resolve_V (Pos : Element_Vectors.Cursor)
      is
      begin
         Element_Vectors.Element (Pos).Resolve;
      end Resolve_V;
   begin
      Messages.Trace ("... checking class " & (+C.Name));
      C.Attributes.Iterate (Resolve_M'Access);
      C.Operations.Iterate (Resolve_V'Access);
      C.State_Machines.Iterate (Resolve_V'Access);
   end Resolve;

   overriding
   procedure Output (C : Class_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<class");
      Output_Class_Aspects (C, To);
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
