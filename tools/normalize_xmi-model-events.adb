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

--  $RCSfile: normalize_xmi-model-events.adb,v $
--  $Revision: de3f2d1bb719 $
--  $Date: 2012/01/19 17:12:05 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;
with Ada.Strings.Fixed;

package body Normalize_XMI.Model.Events is


   procedure Read_Event (From : DOM.Core.Node;
                         Parent : not null Element_P;
                         Accumulating_In : in out Element_Maps.Map)
   is
      use Ada.Text_IO;
      N : constant Element_P := new Event_Element;
      E : Event_Element renames Event_Element (N.all);
   begin
      E.Parent := Parent;
      E.Populate (From => From);
      declare
         --  We don't use any qualifiers present in the element's
         --  name, because they're only there to disambiguate events
         --  with the same name but directed to different classes.
         Name : constant String := Read_Name (From_Element => From);
         Dot : constant Natural
           := Ada.Strings.Fixed.Index (Source => Name,
                                       Pattern => ".",
                                       From => Name'Last,
                                       Going => Ada.Strings.Backward);
      begin
         Put_Line (Standard_Error, "... reading event " & Name);
         if Dot = 0 then
            E.Name := +Name;
         else
            E.Name := +Name (Dot + 1 .. Name'Last);
         end if;
      end;

      --  Parameter Type. We aren't interested in the parameter name,
      --  because ColdFrame only supports a single Event parameter,
      --  and it's translated as a type extension with component name
      --  "Payload".
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:Event.parameter/UML:Parameter/UML:Parameter.type");
      begin
         if DOM.Core.Nodes.Length (Nodes) > 1 then
            Messages.Error
              ("Event "
              & (+E.Name)
                 & " is not permitted to have more than one parameter.");
         elsif DOM.Core.Nodes.Length (Nodes) = 1 then
            E.Parameter_Type := +Read_Name (DOM.Core.Nodes.Item (Nodes, 0));
         end if;
      end;

      Accumulating_In.Insert (Key => Read_Attribute ("xmi.id",
                                                     From_Element => From),
                              New_Item => N);
   end Read_Event;


   overriding
   procedure Resolve (E : in out Event_Element)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "... checking event " & (+E.Name));
   end Resolve;


   overriding
   procedure Output (E : Event_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<event");
      if E.Has_Stereotype ("class") then
         Put (To, " class='true'");
      end if;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+E.Name) & "</name>");
      if +E.Parameter_Type /= "" then
         Put_Line (To, "<type>" & (+E.Parameter_Type) & "</type>");
      end if;
      E.Output_Documentation (To);
      Put_Line (To, "</event>");
   end Output;


   not overriding
   function Parameter_Type (Of_Event : Event_Element) return String
   is
   begin
      return +Of_Event.Parameter_Type;
   end Parameter_Type;

end Normalize_XMI.Model.Events;
