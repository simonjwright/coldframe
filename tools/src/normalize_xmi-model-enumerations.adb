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
with Normalize_XMI.Model.Operations;
with XIA;

package body Normalize_XMI.Model.Enumerations is

   function Read_Enumeration (From   : not null DOM.Core.Node;
                              Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new Enumeration_Element (Parent);
      E : Enumeration_Element renames Enumeration_Element (N.all);
   begin
      E.Populate (From => From);

      --  Literals
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:Enumeration.literal/UML:EnumerationLiteral");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               Literal  : constant String :=
                 Read_Attribute
                   ("name",
                    From_Element =>
                      DOM.Core.Nodes.Item (Nodes, J));
            begin
               if Identifiers.Is_Valid (Literal) then
                  E.Literals.Append (Identifiers.Normalize (Literal));
               else
                  Messages.Error ("Invalid literal """
                                    & Literal
                                    & """ in enumeration "
                                    & E.Fully_Qualified_Name);
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
                                            Parent => N);
               Name : constant String := +O.Name;
            begin
               if E.Operations.Contains (Name) then
                  Messages.Error
                    ("Type "
                       & E.Fully_Qualified_Name
                       & " has duplicate operation "
                       & Name);
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
        and then not E.Has_Tag ("language")
      then
         Messages.Error
           ("Type "
              & E.Fully_Qualified_Name
              & " has <<convention>> but not {language}");
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
      if E.Has_Stereotype ("callback") then
         Put (To, " callback='true'");
      end if;
      if E.Has_Stereotype ("convention") then
         Put (To, " convention='" & E.Tag_Value ("language") & "'");
      end if;
      declare
         Visibility : constant String
           := Read_Attribute ("visibility", From_Element => E.Node);
      begin
         if Visibility = "" or else Visibility = "package" then
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
