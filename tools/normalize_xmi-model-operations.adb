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

--  $RCSfile: normalize_xmi-model-operations.adb,v $
--  $Revision: f3a9cc2c7d9c $
--  $Date: 2014/01/11 14:11:13 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Identifiers;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Parameters;

package body Normalize_XMI.Model.Operations is


   function Read_Operation (From   : not null DOM.Core.Node;
                            Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new Operation_Element (Parent);
      O : Operation_Element renames Operation_Element (N.all);
   begin
      O.Populate (From => From);

      --  Parameters
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From,
            "UML:BehavioralFeature.parameter/UML:Parameter"
              & "[not (@name='return')]");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               P : constant Element_P :=
                 Parameters.Read_Parameter (DOM.Core.Nodes.Item (Nodes, J),
                                            Parent => N);
            begin
               O.Parameters.Append (New_Item => P);
            end;
         end loop;
      end;

      --  Return
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From,
            "UML:BehavioralFeature.parameter/UML:Parameter"
              & "[(@name='return')]"
              & "/UML:Parameter.type/*/@name");
      begin
         if DOM.Core.Nodes.Length (Nodes) /= 0 then
            O.Return_Type :=
              +Identifiers.Normalize
                (DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item (Nodes, 0)));
         end if;
      end;

      return N;
   end Read_Operation;


   overriding
   procedure Resolve (O : in out Operation_Element)
   is
      procedure Resolve (Pos : Element_Vectors.Cursor);
      procedure Resolve (Pos : Element_Vectors.Cursor)
      is
      begin
         Element_Vectors.Element (Pos).Resolve;
      end Resolve;
      use type Ada.Containers.Count_Type;
   begin
      Messages.Trace ("...... checking operation " & (+O.Name));
      if O.Has_Stereotype ("convention")
        and not O.Has_Tag ("language") then
         Messages.Error
           ("Operation "
              & O.Fully_Qualified_Name
              & " has <<convention>> but not {language}");
      end if;
      if O.Has_Stereotype ("entry")
        and Ada.Strings.Unbounded.Length (O.Return_Type) > 0 then
         Messages.Error
           ("Entry "
              & O.Fully_Qualified_Name
              & " can't be a function");
      end if;
      O.Parameters.Iterate (Resolve'Access);
      if O.Has_Stereotype ("callback") then
         if O.Parameters.Length /= 1 then
            Messages.Error
              ("<<callback>> operation "
                 & O.Fully_Qualified_Name
                 & " must have one and only one  parameter");
         end if;
         if Ada.Strings.Unbounded.Length (O.Return_Type) > 0 then
            Messages.Error
              ("<<callback>> operation "
                 & O.Fully_Qualified_Name
                 & " can't be a function");
         end if;
      end if;
   end Resolve;


   overriding
   procedure Output (O : Operation_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      procedure Output (Pos : Element_Vectors.Cursor);
      procedure Output (Pos : Element_Vectors.Cursor)
      is
      begin
         Element_Vectors.Element (Pos).Output (To);
      end Output;
      Modelled_As_Class : constant Boolean
        := Read_Attribute ("ownerScope", From_Element => O.Node)
          = "classifier";
      Forced_To_Class : Boolean := False;
   begin

      Put (To, "<operation");
      declare
         Is_Abstract : constant Boolean
           := Boolean'Value (Read_Attribute ("isAbstract",
                                             From_Element => O.Node));
      begin
         if Is_Abstract then
            Put (To, " abstract='true'");
         end if;
      end;
      if O.Parent.Has_Stereotype ("access-to-operation") then
         Put (To, " access='true'");
      end if;
      if O.Has_Stereotype ("callback") then
         if not Modelled_As_Class then
            Forced_To_Class := True;
            Messages.Warning ("<<callback>> operation "
                                & O.Fully_Qualified_Name
                                & " forced to be a class operation");
         end if;
         Put (To, " callback='true'");
      end if;
      if O.Has_Stereotype ("convention") then
         Put (To, " convention='" & O.Tag_Value ("language") & "'");
      end if;
      if O.Has_Stereotype ("entry") then
         Put (To, " entry='true'");
      end if;
      if O.Has_Stereotype ("final") then
         Put (To, " final='true'");
      end if;
      if O.Has_Stereotype ("init") then
         if not Modelled_As_Class then
            Forced_To_Class := True;
            Messages.Warning ("<<init>> operation "
                                & O.Fully_Qualified_Name
                                & " forced to be a class operation");
         end if;
         Put (To, " initialize='true'");
      end if;
      if O.Has_Tag ("renames") then
         Put (To, " renames='" & O.Tag_Value ("renames") & "'");
      end if;
      if Ada.Strings.Unbounded.Length (O.Return_Type) > 0 then
         Put (To, " return='" & (+O.Return_Type) & "'");
      end if;
      declare
         Visibility : constant String
           := Read_Attribute ("visibility", From_Element => O.Node);
      begin
         --  XXX I've made operation visibility for 'package' 'public'
         --  rather than 'private' as for other occurrences; should we
         --  maybe warn about 'package'?
         if Visibility = "package" then
            Put (To, " visibility='public'");
         elsif Visibility = "" then
            Put (To, " visibility='private'");
         else
            Put (To, " visibility='" & Visibility & "'");
         end if;
      end;
      if Modelled_As_Class or Forced_To_Class then
         Put (To, " class='true'");
      end if;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+O.Name) & "</name>");
      O.Output_Documentation (To);
      O.Parameters.Iterate (Output'Access);
      Put_Line (To, "</operation>");
   end Output;


end Normalize_XMI.Model.Operations;
