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

--  $RCSfile: normalize_xmi-model-associations.adb,v $
--  $Revision: 9f22e3d1ef24 $
--  $Date: 2011/12/28 21:39:57 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Association_Ends;
with Normalize_XMI.Model.Classes;

package body Normalize_XMI.Model.Associations is


   function Read_Association (From : DOM.Core.Node;
                              Parent : not null Element_P) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Association_Element;
      A : Association_Element renames Association_Element (N.all);
   begin
      A.Parent := Parent;
      A.Populate (From => From);
      A.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "...... reading association " & (+A.Name));

      --  Ends
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:Association.connection/UML:AssociationEnd");
      begin
         pragma Assert (DOM.Core.Nodes.Length (Nodes) = 2,
                        "should be 2 AssociationEnds to an Association");
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               E : constant Element_P :=
                 Association_Ends.Read_Association_End
                 (DOM.Core.Nodes.Item (Nodes, J),
                  Parent => N);
            begin
               A.Ends.Append (New_Item => E);
            end;
         end loop;
      end;

      if +A.Ends.Element (1).Name = +A.Ends.Element (2).Name then
         Messages.Error
           ("Association "
              & (+A.Name)
              & "'s ends have the same role name.");
      end if;

      return N;
   end Read_Association;


   overriding
   procedure Resolve (A : in out Association_Element)
   is
      use Ada.Text_IO;
      procedure Resolve (Pos : Element_Vectors.Cursor);
      procedure Resolve (Pos : Element_Vectors.Cursor)
      is
      begin
         Element_Vectors.Element (Pos).Resolve;
      end Resolve;
      E1 : Association_Ends.Association_End_Element
        renames Association_Ends.Association_End_Element
        (A.Ends.Element (1).all);
      C1 : constant Element_P := A.Find_Class (+E1.Participant);
      C1C : Classes.Class_Element renames Classes.Class_Element (C1.all);
      E2 : Association_Ends.Association_End_Element
        renames Association_Ends.Association_End_Element
        (A.Ends.Element (2).all);
      C2 : constant Element_P := A.Find_Class (+E2.Participant);
      C2C : Classes.Class_Element renames Classes.Class_Element (C2.all);
      use Association_Ends;
   begin
      Put_Line (Standard_Error, "...... checking association " & (+A.Name));
      Element_Vectors.Iterate (A.Ends, Resolve'Access);
      if E1.Source and E2.Source then
         Messages.Error
           ("Both ends of association "
              & (+A.Name)
              & " are marked <<source>>.");
      elsif E1.Lower = E2.Lower
        and E1.Upper = E2.Upper
        and not E1.Source and not E2.Source then
         Messages.Error
           ("Neither end of symmetric association "
              & (+A.Name)
              & " is marked <<source>>.");
      else
         case E1.Upper is
            when One =>
               case E2.Upper is
                  when One =>
                     case E1.Lower is
                        when Zero =>
                           case E2.Lower is
                              when Zero =>
                                 --  1c:1c; obey <<source>>
                                 if E1.Source then
                                    --  Formalizing attribute to be
                                    --  added to the class at E2.
                                    C2C.Create_Referential_Attribute
                                      (Referring_To => C1,
                                       For_Relationship => A'Unchecked_Access,
                                       With_Source_Role_Name => +E1.Name,
                                       Forming_Identifier => False);
                                 else
                                    --  Checked above that one end
                                    --  _is_ marked <<source>>.
                                    --  Formalizing attribute to be
                                    --  added to the class at E1.
                                    C1C.Create_Referential_Attribute
                                      (Referring_To => C2,
                                       For_Relationship => A'Unchecked_Access,
                                       With_Source_Role_Name => +E2.Name,
                                       Forming_Identifier => False);
                                 end if;
                              when One =>
                                 --  The formalizing attribute is
                                 --  added to the conditional end,
                                 --  here E1.
                                 if E1.Source then
                                    E1.Source := False;
                                    Messages.Warning
                                      (+A.Name
                                         & "."
                                         & (+E1.Name)
                                         & " is marked <<source>>,"
                                         & " ignored.");
                                 end if;
                                 E2.Source := True;
                                 C1C.Create_Referential_Attribute
                                   (Referring_To => C2,
                                    For_Relationship => A'Unchecked_Access,
                                    With_Source_Role_Name => +E2.Name,
                                    Forming_Identifier => False);
                           end case;
                        when One =>
                           case E2.Lower is
                              when Zero =>
                                 --  The formalizing attribute is
                                 --  added to the conditional end,
                                 --  here E2.
                                 if E2.Source then
                                    E2.Source := False;
                                    Messages.Warning
                                      (+A.Name
                                         & "."
                                         & (+E2.Name)
                                         & " is marked <<source>>,"
                                         & " ignored.");
                                 end if;
                                 E1.Source := True;
                                 C2C.Create_Referential_Attribute
                                   (Referring_To => C1,
                                    For_Relationship => A'Unchecked_Access,
                                    With_Source_Role_Name => +E1.Name,
                                    Forming_Identifier => False);
                              when One =>
                                 --  1c:1c; obey <<source>>
                                 if E1.Source then
                                    --  Formalizing attribute to be
                                    --  added to the class at E2.
                                    C2C.Create_Referential_Attribute
                                      (Referring_To => C1,
                                       For_Relationship => A'Unchecked_Access,
                                       With_Source_Role_Name => +E1.Name,
                                       Forming_Identifier => False);
                                 else
                                    --  Checked above that one end
                                    --  _is_ marked <<source>>.
                                    --  Formalizing attribute to be
                                    --  added to the class at E1.
                                    C1C.Create_Referential_Attribute
                                      (Referring_To => C2,
                                       For_Relationship => A'Unchecked_Access,
                                       With_Source_Role_Name => +E2.Name,
                                       Forming_Identifier => False);
                                 end if;
                           end case;
                     end case;
                  when Many =>
                     --  Formalizing attribute to be added to the
                     --  class at E2.
                     if E2.Source then
                        E2.Source := False;
                        Messages.Warning
                          (+A.Name
                             & "."
                             & (+E2.Name)
                             & " is marked <<source>>,"
                             & " ignored.");
                     end if;
                     E1.Source := True;
                     C2C.Create_Referential_Attribute
                       (Referring_To => C1,
                        For_Relationship => A'Unchecked_Access,
                        With_Source_Role_Name => +E1.Name,
                        Forming_Identifier => False);
               end case;
            when Many =>
               case E2.Upper is
                  when One =>
                     --  Formalizing attribute to be added to the
                     --  class at E1.
                     if E1.Source then
                        E1.Source := False;
                        Messages.Warning
                          (+A.Name
                             & "."
                             & (+E1.Name)
                             & " is marked <<source>>,"
                             & " ignored.");
                     end if;
                     E2.Source := True;
                     C1C.Create_Referential_Attribute
                       (Referring_To => C2,
                        For_Relationship => A'Unchecked_Access,
                        With_Source_Role_Name => +E2.Name,
                        Forming_Identifier => False);
                  when Many =>
                     Messages.Error
                       ("Both ends of plain association "
                          & (+A.Name)
                          & " are multiple; need association class.");
               end case;
         end case;
      end if;
   end Resolve;


   overriding
   procedure Output (A : Association_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      procedure Output (Pos : Element_Vectors.Cursor);
      procedure Output (Pos : Element_Vectors.Cursor)
      is
      begin
         Element_Vectors.Element (Pos).Output (To);
      end Output;
   begin
      Put (To, "<association");
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+A.Name) & "</name>");
      Element_Vectors.Iterate (A.Ends, Output'Access);
      A.Output_Documentation (To);
      --  <associative/>
      Put_Line (To, "</association>");
   end Output;


end Normalize_XMI.Model.Associations;
