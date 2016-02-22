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

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Tags;
with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Identifiers;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Domains;
with Unicode.CES;

package body Normalize_XMI.Model is


   --------------------
   --  Tag checking  --
   --------------------

   type Tag_Properties is record
      Must_Be_Name : Boolean;
   end record;

   package Uncased_Tag_Properties_Maps
     is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => String,
      "<" => Uncased_Less_Than,
      Element_Type => Tag_Properties);

   Tags : Uncased_Tag_Properties_Maps.Map;


   procedure Process_Domain (From : not null DOM.Core.Node; In_File : String)
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

   not overriding
   procedure Populate (E : in out Element; From : DOM.Core.Node)
   is
      Name : constant String := Read_Attribute ("name", From_Element => From);
   begin
      E.Node := From;

      if Identifiers.Is_Valid (Name) then
         E.Name := +Identifiers.Normalize (Name);
      else
         E.Name := +Name;
         Messages.Error ("Invalid "
                           & E.Kind
                           & " name "
                           & E.Fully_Qualified_Name);
      end if;

      declare
         S : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:ModelElement.stereotype/UML:Stereotype/@name");
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
                   (DOM.Core.Nodes.Item (T, J),
                    "UML:TaggedValue.type/UML:TagDefinition/@name");
               Value : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (DOM.Core.Nodes.Item (T, J), "UML:TaggedValue.dataValue");
            begin
               if DOM.Core.Nodes.Length (Tag) = 1
                 and DOM.Core.Nodes.Length (Value) = 1
               then
                  declare
                     T : constant String
                       := DOM.Core.Nodes.Node_Value
                         (DOM.Core.Nodes.Item (Tag, 0));
                     V : constant String
                       := Read_Text (DOM.Core.Nodes.Item (Value, 0));
                  begin
                     if Tags.Contains (T)
                       and then Tags.Element (T).Must_Be_Name
                     then
                        if Identifiers.Is_Valid (V) then
                           E.Tagged_Values.Insert (T,
                                                   Identifiers.Normalize (V));
                        else
                           E.Tagged_Values.Insert (T, V);
                           Messages.Error
                             ("Invalid value in {"
                                & T & "="
                                & V
                                & "} on "
                                & E.Fully_Qualified_Name);
                        end if;
                     else
                        E.Tagged_Values.Insert (T, V);
                     end if;
                  end;
               else
                  if DOM.Core.Nodes.Length (Tag) /= 1 then
                     Messages.Error ("Bad tag name in " & (+E.Name));
                  end if;
                  if DOM.Core.Nodes.Length (Value) /= 1 then
                     Messages.Error ("Bad tag value in " & (+E.Name));
                  end if;
               end if;
            end;
         end loop;
      end;
   end Populate;


   not overriding
   function Fully_Qualified_Name (E : Element) return String
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if E.Parent /= null then
         if E.Name = "" then
            return Fully_Qualified_Name (E.Parent.all) & ".{" & E.Kind & "}";
         else
            return Fully_Qualified_Name (E.Parent.all) & "." & (+E.Name);
         end if;
      else
         return +E.Name;
      end if;
   end Fully_Qualified_Name;


   not overriding
   function Kind (E : Element) return String
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
      Full_Name : constant String
        := Translate (Ada.Tags.Expanded_Name (Element'Class (E)'Tag),
                      Lower_Case_Map);
      Last_Dot : constant Natural
        := Ada.Strings.Fixed.Index (Source => Full_Name,
                                    Pattern => ".",
                                    From => Full_Name'Last,
                                    Going => Ada.Strings.Backward);
      Last_Component : constant String
        := Full_Name (Positive'Max (Last_Dot + 1, Full_Name'First)
                        .. Full_Name'Last);
      --  This code expects all Element types to have names ending in
      --  "_Element". It won't fail if they don't, it just may not
      --  make sense.
      Last_Underscore : constant Natural
        := Ada.Strings.Fixed.Index (Source => Last_Component,
                                    Pattern => "_",
                                    From => Last_Component'Last,
                                    Going => Ada.Strings.Backward);
   begin
      if Last_Underscore = 0 then
         return Last_Component;
      else
         return Last_Component (Last_Component'First .. Last_Underscore - 1);
      end if;
   end Kind;


   not overriding
   function Find_Class (Known_To        : Element;
                        With_Model_Name : String) return Element_P
   is
   begin
      pragma Assert (Known_To.Parent /= null,
                     "Parent reference is null");
      return Known_To.Parent.Find_Class (With_Model_Name);
   end Find_Class;


   not overriding
   function Find_Type (Known_To        : Element;
                       With_Model_Name : String) return Element_P
   is
   begin
      pragma Assert (Known_To.Parent /= null,
                     "Parent reference is null");
      return Known_To.Parent.Find_Type (With_Model_Name);
   end Find_Type;


   not overriding
   function Has_Stereotype (E : Element; Stereotype : String) return Boolean
   is
   begin
      return E.Stereotypes.Contains (Stereotype);
   end Has_Stereotype;


   not overriding
   function Has_Tag (E : Element; Tag : String) return Boolean
   is
   begin
      return E.Tagged_Values.Contains (Tag);
   end Has_Tag;


   not overriding
   function Tag_Value (E : Element; Tag : String) return String
   is
   begin
      if E.Tagged_Values.Contains (Tag) then
         return E.Tagged_Values.Element (Tag);
      else
         return "";
      end if;
   end Tag_Value;


   not overriding
   procedure Output_Documentation (E : Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
      use Ada.Text_IO;
      Doc : constant String := E.Tag_Value ("documentation");
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


   ----------------------
   --  Standard types  --
   ----------------------

   overriding
   procedure Resolve (ST : in out Standard_Type_Element)
   is
   begin
      null;
   end Resolve;


   overriding
   procedure Output (ST : Standard_Type_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put_Line (To, "<type standard='true'>");
      Put_Line (To, "<name>" & (+ST.Name) & "</name>");
      Put_Line (To, "</type>");
   end Output;


   --------------------
   --  XML Utilities --
   --------------------

   function Read_Attribute (Named : String;
                            From_Element : DOM.Core.Node) return String
   is
      P : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
        (From_Element, "@" & Named);
   begin
      if DOM.Core.Nodes.Length (P) = 0 then
         return "";
      else
         return DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item (P, 0));
      end if;
   end Read_Attribute;


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


begin

   --  Initialize Tags (for checking tag values as they are read)
   Tags.Insert ("abbreviation", (Must_Be_Name => True));
   Tags.Insert ("access-to-type", (Must_Be_Name => True));
   Tags.Insert ("constrains", (Must_Be_Name => True));
   Tags.Insert ("documentation", (Must_Be_Name => False));
   Tags.Insert ("formalizes", (Must_Be_Name => True));
   Tags.Insert ("hash", (Must_Be_Name => False));
   Tags.Insert ("imported", (Must_Be_Name => True));
   Tags.Insert ("init", (Must_Be_Name => True));
   Tags.Insert ("language", (Must_Be_Name => False));
   Tags.Insert ("length", (Must_Be_Name => False));
   Tags.Insert ("lower", (Must_Be_Name => False));
   Tags.Insert ("max", (Must_Be_Name => False));
   Tags.Insert ("mod", (Must_Be_Name => False));
   Tags.Insert ("name", (Must_Be_Name => True));
   Tags.Insert ("priority", (Must_Be_Name => False));
   Tags.Insert ("renames", (Must_Be_Name => True));
   Tags.Insert ("revision", (Must_Be_Name => False));
   Tags.Insert ("stack", (Must_Be_Name => False));
   Tags.Insert ("upper", (Must_Be_Name => False));

end Normalize_XMI.Model;
