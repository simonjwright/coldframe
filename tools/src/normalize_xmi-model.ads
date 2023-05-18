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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with DOM.Core;

private package Normalize_XMI.Model is

   procedure Process_Domain (From    : not null DOM.Core.Node;
                             In_File : String);

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

   type Element is tagged;
   type Element_P is access all Element'Class;

   --  A UML Model Element, abstracted to the form required to
   --  generate the ColdFrame normalized XML.
   --
   --  It's made limited because we need to preserve its identity.
   type Element (Parent : Element_P) is abstract tagged limited record
      Node : DOM.Core.Node;
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Stereotypes : Uncased_String_Sets.Set;
      Tagged_Values : Uncased_String_Maps.Map;
   end record;

   --  Fill in the Node, Name, Stereotypes, and Tagged_Values fields.
   --  Needs_Name is False if the Name field isn't required
   --  (e.g. initial/final states, completion transitions).
   not overriding
   procedure Populate (E : in out Element;
                       From : DOM.Core.Node;
                       Needs_Name : Boolean := True);

   --  Determine the full name of the element: for example,
   --  {domain}.{class}.{operation}.{parameter}. Used for error
   --  reporting.
   not overriding
   function Fully_Qualified_Name (E : Element) return String;

   --  Determine the kind of the element (for example, "class",
   --  "operation"). Used for error reporting.
   not overriding
   function Kind (E : Element) return String;

   --  Search up the Parent tree to find a Class which is named
   --  'With_Model_Name' in the model. The point of this is, of
   --  course, to find the Class; the Name shenanigans is because the
   --  Class corresponding to an AssociationClass (what OOA would
   --  think of as the associative class) has to have its .Name
   --  component changed to avoid the clash of names with the
   --  association itself.
   --
   --  Overridden in Domains, which searches its class map, which is
   --  keyed by the original name.
   not overriding
   function Find_Class (Known_To        : Element;
                        With_Model_Name : String) return Element_P;

   --  Similarly for Types.
   not overriding
   function Find_Type (Known_To        : Element;
                       With_Model_Name : String) return Element_P;

   not overriding
   function Has_Stereotype (E : Element; Stereotype : String) return Boolean;

   not overriding
   function Has_Tag (E : Element; Tag : String) return Boolean;

   --  Read the named tagged value (or empty string if not found).
   not overriding
   function Tag_Value (E : Element; Tag : String) return String;

   --  Complete any aspects of the Element that can't be determined by
   --  a simple top-down scan.
   not overriding
   procedure Resolve (E : in out Element) is abstract;

   --  Output the Element and its contents to the open file To, in
   --  normalized XML.
   not overriding
   procedure Output (E : Element; To : Ada.Text_IO.File_Type) is abstract;

   --  Outputs the contents of the "document" tag, split into paragraphs.
   not overriding
   procedure Output_Documentation (E : Element; To : Ada.Text_IO.File_Type);

   package Element_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => String,
      Element_Type => Element_P);

   package Element_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => Element_P);

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => String);

   ----------------------
   --  Standard types  --
   ----------------------

   --  This type is used to hold ColdFrame's predeclared types
   --  (Integer, Date etc).

   type Standard_Type_Element is new Element with null record;

   overriding
   procedure Resolve (ST : in out Standard_Type_Element);

   overriding
   procedure Output (ST : Standard_Type_Element; To : Ada.Text_IO.File_Type);

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

   --  Reads and concatenates the child Text_Nodes of From_Element.
   function Read_Text (From_Element : DOM.Core.Node) return String;

   -------------------------
   --  Output  utilities  --
   -------------------------

   --  Debug.
   procedure Print_Node (N : DOM.Core.Node);

end Normalize_XMI.Model;
