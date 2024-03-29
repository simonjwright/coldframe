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
with XIA;

package body Normalize_XMI.Model.Association_Ends is

   function Read_Association_End
     (From : DOM.Core.Node;
      Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new Association_End_Element (Parent);
      E : Association_End_Element renames Association_End_Element (N.all);
   begin
      E.Populate (From => From);

      --  Lower
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From,
            "UML:AssociationEnd.multiplicity"
              & "/UML:Multiplicity"
              & "/UML:Multiplicity.range"
              & "/UML:MultiplicityRange"
              & "/@lower");
         pragma Assert
           (DOM.Core.Nodes.Length (Nodes) = 1,
            "should be 1 'UML:MultiplicityRange/@lower' child"
              & " of an Association_End");
         Lower : constant String
           := DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item (Nodes, 0));
      begin
         if Lower = "0" then
            E.Lower := Zero;
         elsif Lower = "1" then
            E.Lower := One;
         else
            Messages.Error
              ("Unsupported lower bound '"
                 & Lower
                 & "' in "
                 & E.Fully_Qualified_Name);
         end if;
      end;

      --  Upper
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From,
            "UML:AssociationEnd.multiplicity"
              & "/UML:Multiplicity"
              & "/UML:Multiplicity.range"
              & "/UML:MultiplicityRange"
              & "/@upper");
         pragma Assert
           (DOM.Core.Nodes.Length (Nodes) = 1,
            "should be 1 'UML:MultiplicityRange/@upper' child"
              & " of an Association_End");
         Upper : constant String
           := DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item (Nodes, 0));
      begin
         if Upper = "1" then
            E.Upper := One;
         elsif Upper = "-1" then
            E.Upper := Many;
         else
            Messages.Error
              ("Unsupported upper bound '"
                 & Upper
                 & "' in "
                 & E.Fully_Qualified_Name);
         end if;
      end;

      --  Participant
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:AssociationEnd.participant/*/@name");
         pragma Assert
           (DOM.Core.Nodes.Length (Nodes) = 1,
            "should be 1 'UML:AssociationEnd.participant/*/@name' child"
              & " of an Association_End");
         Name : constant String :=
           DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item (Nodes, 0));
      begin
         if Identifiers.Is_Valid (Name) then
            E.Participant := E.Find_Class (Identifiers.Normalize (Name));
         else
            --  The invalid participant class name will be reported
            --  elsewhere.
            E.Participant := E.Find_Class (Name);
         end if;
         if E.Participant = null then
            Messages.Error
              ("Couldn't find participant class "
                 & Name
                 & " for association end "
                 & E.Fully_Qualified_Name);
         end if;
      end;

      E.Source := E.Has_Stereotype ("source");

      return N;
   end Read_Association_End;

   overriding
   procedure Resolve (E : in out Association_End_Element)
   is
   begin
      Messages.Trace ("...... checking association_end " & (+E.Name));
   end Resolve;

   overriding
   procedure Output (E : Association_End_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<role");
      if E.Lower = Zero then
         Put (To, " conditional='true'");
      end if;
      if E.Upper = Many then
         Put (To, " multiple='true'");
      end if;
      if E.Source then
         Put (To, " source='true'");
      end if;
      Put_Line (To, ">");
      Put_Line (To, "<classname>" & (+E.Participant.Name) & "</classname>");
      Put_Line (To, "<name>" & (+E.Name) & "</name>");
      E.Output_Documentation (To);
      Put_Line (To, "</role>");
   end Output;

end Normalize_XMI.Model.Association_Ends;
