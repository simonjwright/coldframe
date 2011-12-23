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

--  $RCSfile: normalize_xmi-model.ads,v $
--  $Revision: 964643748739 $
--  $Date: 2011/12/23 12:06:03 $
--  $Author: simonjwright $

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with DOM.Core;

private package Normalize_XMI.Model is

   procedure Process_Domain (From : DOM.Core.Node; In_File : String);
   --  XXX not sure how to deal with <<interface>> subpackages.

private

   function Uncased_Equals (L, R : String) return Boolean;
   function Uncased_Less_Than (L, R : String) return Boolean;

   --  For Tagged Values and Case Exceptions.
   package Uncased_String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => String,
      "<" => Uncased_Less_Than,
      Element_Type => String,
      "=" => Uncased_Equals);

   --  For Stereotypes.
   package Uncased_String_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => String,
      "<" => Uncased_Less_Than,
      "=" => Uncased_Equals);

   type Element;
   type Element_P is access all Element'Class;

   --  A UML Model Element, abstracted to the form required to
   --  generate the ColdFrame normalized XML.
   --
   --  It's made limited because we need to preserve its identity.
   type Element is abstract tagged limited record
      Node : DOM.Core.Node;
      Stereotypes : Uncased_String_Sets.Set;
      Tagged_Values : Uncased_String_Maps.Map;
      Parent : Element_P;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Fill in the Node, Stereotypes, and Tagged_Values fields.
   procedure Populate (E : in out Element; From : DOM.Core.Node);

   --  Search up the Parent tree to find a Class 'Named'.
   function Find_Class (Known_To : Element; Named : String) return Element_P;

   function Has_Stereotype (E : Element; Stereotype : String) return Boolean;

   function Has_Tag (E : Element; Tag : String) return Boolean;

   --  Read the named tagged value and normalize it (or empty string
   --  if not found).
   function Tag_As_Name (E : Element; Tag : String) return String;

   --  Read the named tagged value (or empty string if not found).
   function Tag_As_Value (E : Element; Tag : String) return String;

   --  Complete any aspects of the Element that can't be determined by
   --  a simple top-down scan.
   procedure Resolve (E : in out Element) is abstract;

   --  Output the Element and its contents to the open file To, in
   --  normalized XML.
   procedure Output (E : Element; To : Ada.Text_IO.File_Type) is abstract;

   --  Outputs the contents of the "document" tag, split into paragraphs.
   procedure Output_Documentation (E : Element; To : Ada.Text_IO.File_Type);

   package Element_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => String,
      Element_Type => Element_P);

   package Element_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => Element_P);

   ----------------
   --  Utilities --
   ----------------

   function "+" (R : String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (R : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   --------------------
   --  XML Utilities --
   --------------------

   --  Reads the named attribute from the element (returns the empty
   --  string if not found).
   function Read_Attribute (Named : String;
                            From_Element : DOM.Core.Node) return String;

   --  Reads the "name" attribute from the element and normalizes it.
   function Read_Name (From_Element : DOM.Core.Node) return String;

   --  Reads and concatenates the child Text_Nodes of From_Element.
   function Read_Text (From_Element : DOM.Core.Node) return String;

   -------------------------
   --  Output  utilities  --
   -------------------------

   --  Debug.
   procedure Print_Node (N : DOM.Core.Node);

end Normalize_XMI.Model;
