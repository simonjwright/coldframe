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

--  $RCSfile: normalize_xmi-model.adb,v $
--  $Revision: fd881b1b7e39 $
--  $Date: 2011/12/11 15:20:54 $
--  $Author: simonjwright $

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Identifiers;
with Normalize_XMI.Model.Domains;
with Unicode.CES;

package body Normalize_XMI.Model is

   procedure Process_Domain (From : DOM.Core.Node; In_File : String)
     renames Domains.Process_Domain;

   --------------------
   --  XML Utilities --
   --------------------

   function Is_Stereotype_Present (Named : String;
                                   In_Element : DOM.Core.Node) return Boolean
   is
      P : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
        (In_Element,
         "UML:ModelElement.stereotype[@name='" & Named & "']");
   begin
      return DOM.Core.Nodes.Length (P) > 0;
   end Is_Stereotype_Present;


   function Read_Name (From_Element : DOM.Core.Node) return String
   is
      P : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
        (From_Element, "@name");
   begin
      if DOM.Core.Nodes.Length (P) = 0 then
         return "";
      else
         return Identifiers.Normalize (DOM.Core.Nodes.Node_Value
                                         (DOM.Core.Nodes.Item (P, 0)));
      end if;
   end Read_Name;


   function Read_Tagged_Name (Tag : String;
                              From_Element : DOM.Core.Node) return String
   is
      P : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
        (From_Element,
         "UML:ModelElement.taggedValue/UML:TaggedValue"
           & "[UML:TaggedValue.type/@name='"
           & Tag
           & "']"
           & "/UML:TaggedValue.dataValue");
   begin
      if DOM.Core.Nodes.Length (P) = 0 then
         return "";
      else
         --  This is an Element_Node containing a number of Text_Nodes.
         declare
            use Ada.Strings.Unbounded;
            Result : Unbounded_String;
            Children : constant DOM.Core.Node_List
              := DOM.Core.Nodes.Child_Nodes (DOM.Core.Nodes.Item (P, 0));
         begin
            for J in 0 .. DOM.Core.Nodes.Length (Children) - 1 loop
               Append
                 (Result,
                  DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item
                                               (Children, J)));
            end loop;
            return Identifiers.Normalize (+Result);
         end;
      end if;
   end Read_Tagged_Name;


   function Read_Tagged_Value (Tag : String;
                               From_Element : DOM.Core.Node) return String
   is
      P : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
        (From_Element,
         "UML:ModelElement.taggedValue/UML:TaggedValue"
           & "[UML:TaggedValue.type/@name='"
           & Tag
           & "']"
           & "/UML:TaggedValue.dataValue");
   begin
      if DOM.Core.Nodes.Length (P) = 0 then
         return "";
      else
         --  This is an Element_Node containing a number of Text_Nodes.
         declare
            use Ada.Strings.Unbounded;
            Result : Unbounded_String;
            Children : constant DOM.Core.Node_List
              := DOM.Core.Nodes.Child_Nodes (DOM.Core.Nodes.Item (P, 0));
         begin
            for J in 0 .. DOM.Core.Nodes.Length (Children) - 1 loop
               Append
                 (Result,
                  DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item
                                               (Children, J)));
            end loop;
            return +Result;
         end;
      end if;
   end Read_Tagged_Value;


   -------------------------
   --  Output  utilities  --
   -------------------------

   procedure Output_Documentation (S : String; To : Ada.Text_IO.File_Type)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
      use Ada.Text_IO;
      CRLF : constant Character_Set := To_Set (ASCII.CR & ASCII.LF);
      First : Positive := S'First;
      Last : Natural;
   begin
      Put_Line (To, "<documentation>");
      --  We're going to use Find_Token, using a Character_Set with
      --  CR/LF and Membership of Outside, to locate the lines, and
      --  then trim each line (from the right only). Any remaining
      --  non-empty lines get output as <par/> elements.
      loop
         Find_Token (S (First .. S'Last),
                     Set => CRLF,
                     Test => Outside,
                     First => First,
                     Last => Last);
         exit when Last < First;
         declare
            Line : constant String
              := Trim (S (First .. Last), Side => Right);
         begin
            if Line'Length > 0 then
               Put (To, "<par><![CDATA[");
               Put (To, Line);
               Put_Line (To, "]]></par>");
            end if;
         end;
         --  On to the next candidate.
         First := Last + 1;
         exit when First > S'Last;
      end loop;
      Put_Line (To, "</documentation>");
   end Output_Documentation;


   -------------
   --  Debug  --
   -------------

   procedure Print_Node (N : DOM.Core.Node)
   is
      use Ada.Text_IO;
      procedure Print_Text_Node (T : DOM.Core.Text;
                                 Indent : Boolean := False);
      procedure Print_Text_Node (T : DOM.Core.Text;
                                 Indent : Boolean := False)
      is
         White_Space     : constant Unicode.CES.Byte_Sequence
           := ' ' & ASCII.LF & ASCII.CR & ASCII.HT;
         White_Space_Set : constant Ada.Strings.Maps.Character_Set
           := Ada.Strings.Maps.To_Set (White_Space);
         S : constant Unicode.CES.Byte_Sequence
           := Ada.Strings.Fixed.Trim (DOM.Core.Nodes.Node_Value (T),
                                      White_Space_Set,
                                      White_Space_Set);
      begin
         if S'Length > 0 then
            if Indent then
               Put ("  ");
            end if;
            Put_Line (S);
         end if;
      end Print_Text_Node;
      Child : DOM.Core.Node;
      Children : DOM.Core.Node_List;
      Children_Printed : Boolean := False;
      use type DOM.Core.Node_Types;
   begin
      if N.Node_Type = DOM.Core.Element_Node then
         Put ("<");
         Put (DOM.Core.Nodes.Node_Name (N));
         Put (">");
         Children := DOM.Core.Nodes.Child_Nodes (N);
         Children_Printed := False;
         for J in 0 .. DOM.Core.Nodes.Length (Children) - 1 loop
            Child := DOM.Core.Nodes.Item (Children, J);
            if Child.Node_Type = DOM.Core.Element_Node then
               if not Children_Printed then
                  New_Line;
                  Children_Printed := True;
               end if;
               Put ("  <");
               Put (DOM.Core.Nodes.Node_Name (Child));
               Put_Line (">");
            elsif Child.Node_Type = DOM.Core.Text_Node then
               if not Children_Printed then
                  New_Line;
                  Children_Printed := True;
               end if;
               Print_Text_Node (Child, Indent => True);
            end if;
         end loop;
         Put ("</");
         Put (DOM.Core.Nodes.Node_Name (N));
         Put_Line (">");
      elsif N.Node_Type = DOM.Core.Attribute_Node then
         Put (DOM.Core.Nodes.Node_Name (N) & "=""");
         Put (DOM.Core.Nodes.Node_Value (N));
         Put_Line ("""");
      elsif N.Node_Type = DOM.Core.Text_Node then
         Print_Text_Node (N);
      else
         Put (DOM.Core.Nodes.Node_Value (N));
      end if;
   end Print_Node;


end Normalize_XMI.Model;
