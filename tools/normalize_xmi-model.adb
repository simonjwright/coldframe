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
--  $Revision: 2e42ac7f6e38 $
--  $Date: 2011/12/13 17:12:41 $
--  $Author: simonjwright $

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Identifiers;
with Normalize_XMI.Model.Domains;
with Unicode.CES;

package body Normalize_XMI.Model is


   procedure Process_Domain (From : DOM.Core.Node; In_File : String)
     renames Domains.Process_Domain;


   -------------------------
   --  Container support  --
   -------------------------

   function Uncased_Equals (L, R : String) return Boolean
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
   begin
      return Translate (L, Lower_Case_Map) = Translate (R, Lower_Case_Map);
   end Uncased_Equals;


   function Uncased_Less_Than (L, R : String) return Boolean
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
   begin
      return Translate (L, Lower_Case_Map) < Translate (R, Lower_Case_Map);
   end Uncased_Less_Than;


   --------------------------
   --  Element operations  --
   --------------------------

   procedure Populate (E : in out Element; From : DOM.Core.Node)
   is
   begin
      E.Node := From;

      declare
         S : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:ModelElement.stereotype/@name");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (S) - 1 loop
            E.Stereotypes.Insert
              (DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item (S, J)));
         end loop;
      end;

      declare
         T : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:ModelElement.taggedValue/UML:TaggedValue");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (T) - 1 loop
            declare
               Tag : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                 (DOM.Core.Nodes.Item (T, J), "UML:TaggedValue.type/@name");
               Value : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                 (DOM.Core.Nodes.Item (T, J), "UML:TaggedValue.dataValue");
            begin
               E.Tagged_Values.Insert
                 (DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item (Tag, 0)),
                  Read_Text (DOM.Core.Nodes.Item (Value, 0)));
            end;
         end loop;
      end;
   end Populate;


   function Has_Stereotype (E : Element; Stereotype : String) return Boolean
   is
   begin
      return E.Stereotypes.Contains (Stereotype);
   end Has_Stereotype;


   function Has_Tag (E : Element; Tag : String) return Boolean
   is
   begin
      return E.Tagged_Values.Contains (Tag);
   end Has_Tag;


   function Tag_As_Name (E : Element; Tag : String) return String
   is
   begin
      if E.Tagged_Values.Contains (Tag) then
         return Identifiers.Normalize (E.Tagged_Values.Element (Tag));
      else
         return "";
      end if;
   end Tag_As_Name;


   function Tag_As_Value (E : Element; Tag : String) return String
   is
   begin
      if E.Tagged_Values.Contains (Tag) then
         return E.Tagged_Values.Element (Tag);
      else
         return "";
      end if;
   end Tag_As_Value;


   procedure Output_Documentation (E : Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
      use Ada.Text_IO;
      Doc : constant String := E.Tag_As_Value ("documentation");
      CRLF : constant Character_Set := To_Set (ASCII.CR & ASCII.LF);
      First : Positive := Doc'First;
      Last : Natural;
   begin
      Put_Line (To, "<documentation>");
      --  We're going to use Find_Token, using a Character_Set with
      --  CR/LF and Membership of Outside, to locate the lines, and
      --  then trim each line (from the right only). Any remaining
      --  non-empty lines get output as <par/> elements.
      loop
         Find_Token (Doc (First .. Doc'Last),
                     Set => CRLF,
                     Test => Outside,
                     First => First,
                     Last => Last);
         exit when Last < First;
         declare
            Line : constant String
              := Trim (Doc (First .. Last), Side => Right);
         begin
            if Line'Length > 0 then
               Put (To, "<par><![CDATA[");
               Put (To, Line);
               Put_Line (To, "]]></par>");
            end if;
         end;
         --  On to the next candidate.
         First := Last + 1;
         exit when First > Doc'Last;
      end loop;
      Put_Line (To, "</documentation>");
   end Output_Documentation;


   --------------------
   --  XML Utilities --
   --------------------

   function Read_Text (From_Element : DOM.Core.Node) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
      Children : constant DOM.Core.Node_List
        := DOM.Core.Nodes.Child_Nodes (From_Element);
   begin
      for J in 0 .. DOM.Core.Nodes.Length (Children) - 1 loop
         Append
           (Result,
            DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item (Children, J)));
      end loop;
      return +Result;
   end Read_Text;


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


   -------------------------
   --  Output  utilities  --
   -------------------------

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
