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

--  $RCSfile: normalize_xmi-model-classes.ads,v $
--  $Revision: ba36451da4c7 $
--  $Date: 2011/12/26 18:37:18 $
--  $Author: simonjwright $

private package Normalize_XMI.Model.Classes is

   function Read_Class (From : DOM.Core.Node;
                        Parent : not null Element_P) return Element_P;

   type Class_Element is new Element with private;

   overriding
   procedure Resolve (C : in out Class_Element);

   overriding
   procedure Output (C : Class_Element; To : Ada.Text_IO.File_Type);

   --  Create referential attributes to formalize relationships (both
   --  associations and generalizations).
   --
   --  Referring_To is the class that's the source of the referential
   --  attribute (for associations, this is usually the one at the
   --  other end of the association from the class containing the
   --  referential attribute; for generalizations, it's the parent).
   --
   --  For_Relationship is the relationship that's being implemented.
   --
   --  With_Source_Role_Name is the role fulfilled by the source class
   --  (the object in the Shlaer-Mellor formulation
   --  subject->role_name->object). For generalizations, this will be
   --  "Parent".
   --
   --  Forming_Identifier is True if the attribute is to form (part
   --  of) the identifier of the owning class; this will be the case
   --  for associative classes and for children in generalizations.
   not overriding
   procedure Create_Referential_Attribute
     (In_Class : in out Class_Element;
      Referring_To : Element_P;
      For_Relationship : Element_P;
      With_Source_Role_Name : String;
      Forming_Identifier : Boolean);

private

   type Class_Element is new Element with record
      Abbreviation : Ada.Strings.Unbounded.Unbounded_String;
      Attributes : Element_Maps.Map;
      Operations : Element_Maps.Map;
   end record;

   type Referential_Attribute_Element is new Element with record
      Referring_To : Element_P;
      For_Relationship : Element_P;
      With_Source_Role_Name : Ada.Strings.Unbounded.Unbounded_String;
      Identifier : Boolean;
   end record;
   overriding
   procedure Resolve (R : in out Referential_Attribute_Element) is null;
   overriding
   procedure Output (R : Referential_Attribute_Element;
                     To : Ada.Text_IO.File_Type);

end Normalize_XMI.Model.Classes;
