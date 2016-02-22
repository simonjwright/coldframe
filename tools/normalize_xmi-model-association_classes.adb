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
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Association_Ends;

package body Normalize_XMI.Model.Association_Classes is


   function Read_Association_Class
     (From   : not null DOM.Core.Node;
      Parent : not null Element_P) return Element_P
   is
      N : constant Element_P
        := new Association_Class_Element (Parent);
      AC : Association_Class_Element renames Association_Class_Element (N.all);
   begin
      Classes.Populate_Class_Aspects (N, From);
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
               AC.Ends.Append (New_Item => E);
            end;
         end loop;
      end;

      if +AC.Ends.Element (1).Name = +AC.Ends.Element (2).Name then
         Messages.Error
           ("Association Class "
              & AC.Fully_Qualified_Name
              & "'s ends have the same role name");
      end if;
      return N;
   end Read_Association_Class;


   overriding
   procedure Resolve (AC : in out Association_Class_Element)
   is
      procedure Resolve (Pos : Element_Vectors.Cursor);
      procedure Resolve (Pos : Element_Vectors.Cursor)
      is
      begin
         Element_Vectors.Element (Pos).Resolve;
      end Resolve;
   begin
      Messages.Trace ("... checking association class " & (+AC.Name));
      AC.Ends.Iterate (Resolve'Access);
      declare
         E1 : Association_Ends.Association_End_Element
           renames Association_Ends.Association_End_Element
           (AC.Ends.Element (1).all);
         E2 : Association_Ends.Association_End_Element
           renames Association_Ends.Association_End_Element
           (AC.Ends.Element (2).all);
         use Association_Ends;
      begin
         if E1.Source and E2.Source then
            Messages.Error
              ("Both ends of association "
                 & AC.Fully_Qualified_Name
                 & " are marked <<source>>");
         elsif E1.Lower = E2.Lower
           and E1.Upper = One and E2.Upper = One
           and not E1.Source and not E2.Source
         then
            Messages.Error
              ("Neither end of symmetric 1-(1:1) association "
                 & AC.Fully_Qualified_Name
                 & " is marked <<source>>");
         else
            case E1.Upper is
               when One =>
                  case E2.Upper is
                     when One =>
                        case E1.Lower is
                           when Zero =>
                              case E2.Lower is
                                 when Zero =>
                                    --  1-(1c:1c); obey <<source>>
                                    if E1.Source then
                                       --  The identifying formalizing
                                       --  attribute is taken from end
                                       --  1.
                                       AC.Create_Referential_Attribute
                                         (Referring_To => E1.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E1.Name,
                                          Forming_Identifier => True);
                                       AC.Create_Referential_Attribute
                                         (Referring_To => E2.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E2.Name,
                                          Forming_Identifier => False);
                                    else
                                       --  E2.Source; checked above
                                       --  that one end _is_ marked
                                       --  <<source>>.  The
                                       --  identifying formalizing
                                       --  attribute is taken from end
                                       --  2.
                                       AC.Create_Referential_Attribute
                                         (Referring_To => E1.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E1.Name,
                                          Forming_Identifier => False);
                                       AC.Create_Referential_Attribute
                                         (Referring_To => E2.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E2.Name,
                                          Forming_Identifier => True);
                                    end if;
                                 when One =>
                                    --  1-(1c:1)
                                    --  The identifying formalizing
                                    --  attribute is taken from end 2.
                                    if E1.Source then
                                       E1.Source := False;
                                       Messages.Warning
                                         (E1.Fully_Qualified_Name
                                            & " is marked <<source>>,"
                                            & " ignored");
                                    end if;
                                    E2.Source := True;
                                    AC.Create_Referential_Attribute
                                      (Referring_To => E1.Participant,
                                       For_Relationship =>
                                         AC'Unchecked_Access,
                                       With_Source_Role_Name => +E1.Name,
                                       Forming_Identifier => False);
                                    AC.Create_Referential_Attribute
                                      (Referring_To => E2.Participant,
                                       For_Relationship =>
                                         AC'Unchecked_Access,
                                       With_Source_Role_Name => +E2.Name,
                                       Forming_Identifier => True);
                              end case;
                           when One =>
                              case E2.Lower is
                                 when Zero =>
                                    --  1-(1:1c)
                                    --  The identifying formalizing
                                    --  attribute is taken from end 1.
                                    if E2.Source then
                                       E2.Source := False;
                                       Messages.Warning
                                         (E2.Fully_Qualified_Name
                                            & " is marked <<source>>,"
                                            & " ignored");
                                    end if;
                                    E1.Source := True;
                                    AC.Create_Referential_Attribute
                                      (Referring_To => E1.Participant,
                                       For_Relationship =>
                                         AC'Unchecked_Access,
                                       With_Source_Role_Name => +E1.Name,
                                       Forming_Identifier => True);
                                    AC.Create_Referential_Attribute
                                      (Referring_To => E2.Participant,
                                       For_Relationship =>
                                         AC'Unchecked_Access,
                                       With_Source_Role_Name => +E2.Name,
                                       Forming_Identifier => False);
                                 when One =>
                                    --  1-(1:1); obey <<source>>
                                    if E1.Source then
                                       --  The identifying formalizing
                                       --  attribute is taken from end
                                       --  1.
                                       AC.Create_Referential_Attribute
                                         (Referring_To => E1.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E1.Name,
                                          Forming_Identifier => True);
                                       AC.Create_Referential_Attribute
                                         (Referring_To => E2.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E2.Name,
                                          Forming_Identifier => False);
                                    else
                                       --  E2.Source; checked above
                                       --  that one end _is_ marked
                                       --  <<source>>.  The
                                       --  identifying formalizing
                                       --  attribute is taken from end
                                       --  2.
                                       AC.Create_Referential_Attribute
                                         (Referring_To => E1.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E1.Name,
                                          Forming_Identifier => False);
                                       AC.Create_Referential_Attribute
                                         (Referring_To => E2.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E2.Name,
                                          Forming_Identifier => True);
                                    end if;
                              end case;
                        end case;
                     when Many =>
                        --  1-(1:*) or 1-(1c:*)
                        --  The identifying formalizing attribute is
                        --  taken from the many end (2).
                        AC.Create_Referential_Attribute
                          (Referring_To => E1.Participant,
                           For_Relationship =>
                             AC'Unchecked_Access,
                           With_Source_Role_Name => +E1.Name,
                           Forming_Identifier => False);
                        AC.Create_Referential_Attribute
                          (Referring_To => E2.Participant,
                           For_Relationship =>
                             AC'Unchecked_Access,
                           With_Source_Role_Name => +E2.Name,
                           Forming_Identifier => True);
                  end case;
               when Many =>
                  case E2.Upper is
                     when One =>
                        --  1-(*:1) or 1-(*:1c)
                        --  The identifying formalizing attribute is
                        --  taken from the many end (1).
                        AC.Create_Referential_Attribute
                          (Referring_To => E1.Participant,
                           For_Relationship =>
                             AC'Unchecked_Access,
                           With_Source_Role_Name => +E1.Name,
                           Forming_Identifier => True);
                        AC.Create_Referential_Attribute
                          (Referring_To => E2.Participant,
                           For_Relationship =>
                             AC'Unchecked_Access,
                           With_Source_Role_Name => +E2.Name,
                           Forming_Identifier => False);
                     when Many =>
                        --  1-(*:*)
                        --  The identifying formalizing attributes are
                        --  taken from both ends.
                        AC.Create_Referential_Attribute
                          (Referring_To => E1.Participant,
                           For_Relationship =>
                             AC'Unchecked_Access,
                           With_Source_Role_Name => +E1.Name,
                           Forming_Identifier => True);
                        AC.Create_Referential_Attribute
                          (Referring_To => E2.Participant,
                           For_Relationship =>
                             AC'Unchecked_Access,
                           With_Source_Role_Name => +E2.Name,
                           Forming_Identifier => True);
                  end case;
            end case;
         end if;
      end;
   end Resolve;


   overriding
   procedure Output (AC : Association_Class_Element;
                     To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      procedure Output (Pos : Element_Vectors.Cursor);
      procedure Output (Pos : Element_Vectors.Cursor)
      is
      begin
         Element_Vectors.Element (Pos).Output (To);
      end Output;
   begin
      Put (To, "<class");
      Classes.Output_Class_Aspects (AC, To);
      Put_Line (To, "<associative>");
      AC.Ends.Iterate (Output'Access);
      Put_Line (To, "</associative>");
      Put_Line (To, "</class>");
   end Output;


end Normalize_XMI.Model.Association_Classes;
