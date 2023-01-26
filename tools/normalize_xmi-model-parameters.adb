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
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Type_References;
with XIA;

package body Normalize_XMI.Model.Parameters is


   function Read_Parameter (From   : not null DOM.Core.Node;
                            Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new Parameter_Element (Parent);
      P : Parameter_Element renames Parameter_Element (N.all);
   begin
      P.Populate (From => From);

      --  Type
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:Parameter.type/*");
      begin
         if DOM.Core.Nodes.Length (Nodes) = 0 then
            Messages.Error ("No type specified for parameter "
                              & P.Fully_Qualified_Name);
         else
            P.Parameter_Type :=
              Type_References.Read_Type_Reference
              (DOM.Core.Nodes.Item (Nodes, 0),
               Parent => N);
         end if;
      end;

      --  Default value
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:Parameter.defaultValue/UML:Expression");
      begin
         if DOM.Core.Nodes.Length (Nodes) > 0 then
            P.Default_Value :=
              +Read_Attribute ("body",
                               From_Element => DOM.Core.Nodes.Item (Nodes, 0));
         end if;
      end;

      return N;
   end Read_Parameter;


   overriding
   procedure Resolve (P : in out Parameter_Element)
   is
   begin
      Messages.Trace ("......... checking parameter " & (+P.Name));
      --  XXX should check <<not-null>> validity.
   end Resolve;


   overriding
   procedure Output (P : Parameter_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<parameter");
      declare
         Kind : constant String
           := Read_Attribute ("kind", From_Element => P.Node);
      begin
         if Kind = "in" then
            null;  -- As for Ada, we default this.
         elsif Kind = "out" then
            Put (To, " mode='out'");
         elsif Kind = "inout" then
            Put (To, " mode='inout'");
         else
            Messages.Error ("unrecognised ""@kind='"
                              & Kind
                              & "'"" in parameter "
                              & P.Fully_Qualified_Name);
         end if;
         if P.Has_Stereotype ("not-null") then
            Put (To, " not-null='true'");
         end if;
      end;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+P.Name) & "</name>");
      Put_Line (To, "<type>"
                  & Type_References.Type_Reference_Element
                       (P.Parameter_Type.all).Type_Name
                  & "</type>");
      if +P.Default_Value /= "" then
         Put_Line (To, "<default>" & (+P.Default_Value) & "</default>");
      end if;
      P.Output_Documentation (To);
      Put_Line (To, "</parameter>");
   end Output;


end Normalize_XMI.Model.Parameters;
