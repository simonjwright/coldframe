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

--  $RCSfile: normalize_xmi-model-attributes.ads,v $
--  $Revision: 5eca11a43724 $
--  $Date: 2011/12/14 18:22:37 $
--  $Author: simonjwright $

private package Normalize_XMI.Model.Attributes is

   function Read_Attribute (From : DOM.Core.Node) return Element_P;

private

   type Attribute_Element is new Element with record
      Type_Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   overriding
   procedure Resolve (A : in out Attribute_Element);
   overriding
   procedure Output (A : Attribute_Element; To : Ada.Text_IO.File_Type);

end Normalize_XMI.Model.Attributes;
