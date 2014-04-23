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

--  $RCSfile: normalize_xmi-model-association_classes.adb,v $
--  $Revision: eff210d5f78e $
--  $Date: 2014/04/23 16:32:36 $
--  $Author: simonjwright $

with Normalize_XMI.Messages;
with Normalize_XMI.Model.Association_Ends;
with Normalize_XMI.Model.Classes;

package body Normalize_XMI.Model.Association_Classes is


   function Read_Association_Class
     (From   : not null DOM.Core.Node;
      Parent : not null Element_P) return Element_P
   is
      N : constant Element_P
        := new Association_Class_Element (Parent => Parent);
   begin
      Associations.Populate_Association_Aspects (N, From);
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
      Class_Name : constant String := +AC.Name;
   begin
      --  We could have adjusted the names while reading, because we
      --  know (at the time of first coding!!) that Domains reads
      --  Classes before AssociationClasses ... but not a good idea
      --  for the long term.
      AC.Class := AC.Find_Class (Class_Name);
      if AC.Class.Has_Tag ("association-class-name") then
         Messages.Warning
           ("Deprecated <<association-class-name>> on "
              & AC.Fully_Qualified_Name);
      end if;
      case AC.Class.Has_Tag ("association-name") is
         when False =>
            case AC.Class.Has_Tag ("class-name") is
               when False =>
                  if AC.Class.Has_Tag ("association-class-name") then
                     AC.Class.Name :=
                       +(AC.Class.Tag_Value ("association-class-name"));
                  else
                     AC.Class.Name := +(Class_Name & "_Class");
                  end if;
               when True =>
                  AC.Class.Name := +(AC.Class.Tag_Value ("class-name"));
            end case;
         when True =>
            AC.Name := +(AC.Tag_Value ("association-name"));
            case AC.Class.Has_Tag ("class-name") is
               when False =>
                  null;
               when True =>
                  AC.Class.Name := +(AC.Class.Tag_Value ("class-name"));
            end case;
      end case;
      Messages.Trace ("... checking association " & (+AC.Name));
      AC.Ends.Iterate (Resolve'Access);
      declare
         Assoc : Classes.Class_Element
           renames Classes.Class_Element (AC.Class.all);
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
           and not E1.Source and not E2.Source then
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
                                       Assoc.Create_Referential_Attribute
                                         (Referring_To => E1.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E1.Name,
                                          Forming_Identifier => True);
                                       Assoc.Create_Referential_Attribute
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
                                       Assoc.Create_Referential_Attribute
                                         (Referring_To => E1.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E1.Name,
                                          Forming_Identifier => False);
                                       Assoc.Create_Referential_Attribute
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
                                    Assoc.Create_Referential_Attribute
                                      (Referring_To => E1.Participant,
                                       For_Relationship =>
                                         AC'Unchecked_Access,
                                       With_Source_Role_Name => +E1.Name,
                                       Forming_Identifier => False);
                                    Assoc.Create_Referential_Attribute
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
                                    Assoc.Create_Referential_Attribute
                                      (Referring_To => E1.Participant,
                                       For_Relationship =>
                                         AC'Unchecked_Access,
                                       With_Source_Role_Name => +E1.Name,
                                       Forming_Identifier => True);
                                    Assoc.Create_Referential_Attribute
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
                                       Assoc.Create_Referential_Attribute
                                         (Referring_To => E1.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E1.Name,
                                          Forming_Identifier => True);
                                       Assoc.Create_Referential_Attribute
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
                                       Assoc.Create_Referential_Attribute
                                         (Referring_To => E1.Participant,
                                          For_Relationship =>
                                            AC'Unchecked_Access,
                                          With_Source_Role_Name => +E1.Name,
                                          Forming_Identifier => False);
                                       Assoc.Create_Referential_Attribute
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
                        Assoc.Create_Referential_Attribute
                          (Referring_To => E1.Participant,
                           For_Relationship =>
                             AC'Unchecked_Access,
                           With_Source_Role_Name => +E1.Name,
                           Forming_Identifier => False);
                        Assoc.Create_Referential_Attribute
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
                        Assoc.Create_Referential_Attribute
                          (Referring_To => E1.Participant,
                           For_Relationship =>
                             AC'Unchecked_Access,
                           With_Source_Role_Name => +E1.Name,
                           Forming_Identifier => True);
                        Assoc.Create_Referential_Attribute
                          (Referring_To => E2.Participant,
                           For_Relationship =>
                             AC'Unchecked_Access,
                           With_Source_Role_Name => +E2.Name,
                           Forming_Identifier => False);
                     when Many =>
                        --  1-(*:*)
                        --  The identifying formalizing attributes are
                        --  taken from both ends.
                        Assoc.Create_Referential_Attribute
                          (Referring_To => E1.Participant,
                           For_Relationship =>
                             AC'Unchecked_Access,
                           With_Source_Role_Name => +E1.Name,
                           Forming_Identifier => True);
                        Assoc.Create_Referential_Attribute
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
      Put (To, "<association");
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+AC.Name) & "</name>");
      AC.Ends.Iterate (Output'Access);
      AC.Output_Documentation (To);
      Put_Line (To, "<associative>" & (+AC.Class.Name) & "</associative>");
      Put_Line (To, "</association>");
   end Output;


end Normalize_XMI.Model.Association_Classes;
