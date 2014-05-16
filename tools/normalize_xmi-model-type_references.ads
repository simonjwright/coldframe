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

private package Normalize_XMI.Model.Type_References is

   --  A Type_Reference_Element is used for parameter or attribute
   --  types.
   --
   --  A type in the model can be of several different kinds
   --  (enumerations, datatypes, classes).
   --
   --  What we're normally interested in is the name. However, in the
   --  case that the type is actually an AssociationClass, ColdFrame
   --  splits the AssociationClass into two: a <class>, and an
   --  <association> containing an <associative> element holding the
   --  name of the class.
   --
   --  The two objects have to have different names, and ColdFrame
   --  adjusts the name of one or other (or both!) of them depending
   --  on <<association-class-naming>> and {class-name} and/or
   --  {association-name}.
   --
   --  This means that the name in the model (after chasing the
   --  xmi.idref links) is the name of the association, but what we
   --  want to output is the name of the associative class
   --  (associations can't correspond to types).

   function Read_Type_Reference (From   : not null DOM.Core.Node;
                                 Parent : not null Element_P) return Element_P;

   type Type_Reference_Element is new Element with private;

   --  Returns the appropriate name.
   not overriding
   function Type_Name (T : Type_Reference_Element) return String;

private

   type Type_Reference_Element is new Element with null record;
   overriding
   procedure Resolve (T : in out Type_Reference_Element) is null;
   overriding
   procedure Output (T  : Type_Reference_Element;
                     To : Ada.Text_IO.File_Type) is null;

end Normalize_XMI.Model.Type_References;
