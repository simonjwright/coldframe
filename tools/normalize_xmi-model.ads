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
--  $Revision: fd881b1b7e39 $
--  $Date: 2011/12/11 15:20:54 $
--  $Author: simonjwright $

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with DOM.Core;

private package Normalize_XMI.Model is

   procedure Process_Domain (From : DOM.Core.Node; In_File : String);
   --  XXX not sure how to deal with <<interface>> subpackages.

private

   type Element;
   type Element_P is access all Element'Class;

   --  A UML Model Element, abstracted to the form required to
   --  generate the ColdFrame normalized XML.
   --
   --  It's made limited because we need to preserve its identity.
   type Element is abstract tagged limited record
      Parent : Element_P;
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Documentation : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Complete any aspects of the Element that can't be determined by
   --  a simple top-down scan.
   procedure Resolve (E : in out Element) is abstract;

   --  Outputs the Element and its contents to the open file To, in
   --  normalized XML.
   procedure Output (E : Element; To : Ada.Text_IO.File_Type) is abstract;

   package Element_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => String,
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

   function Is_Stereotype_Present (Named : String;
                                   In_Element : DOM.Core.Node) return Boolean;

   --  Reads the normalized "name" attribute from the element.
   function Read_Name (From_Element : DOM.Core.Node) return String;

   --  Reads the named tagged value from the element and normalizes it.
   function Read_Tagged_Name (Tag : String;
                              From_Element : DOM.Core.Node) return String;

   --  Reads the named tagged value from the element.
   function Read_Tagged_Value (Tag : String;
                               From_Element : DOM.Core.Node) return String;

   -------------------------
   --  Output  utilities  --
   -------------------------

   procedure Output_Documentation (S : String; To : Ada.Text_IO.File_Type);

   --  Debug.
   procedure Print_Node (N : DOM.Core.Node);

end Normalize_XMI.Model;
