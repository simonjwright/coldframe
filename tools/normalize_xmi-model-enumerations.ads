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

--  $RCSfile: normalize_xmi-model-enumerations.ads,v $
--  $Revision: 4832d3f648a3 $
--  $Date: 2012/01/25 15:17:08 $
--  $Author: simonjwright $

private package Normalize_XMI.Model.Enumerations is

   function Read_Enumeration (From   : not null DOM.Core.Node;
                              Parent : not null Element_P) return Element_P;

private

   type Enumeration_Element is new Element with record
      Literals : String_Vectors.Vector;
      Operations : Element_Maps.Map;
   end record;
   overriding
   procedure Resolve (E : in out Enumeration_Element);
   overriding
   procedure Output (E : Enumeration_Element; To : Ada.Text_IO.File_Type);

end Normalize_XMI.Model.Enumerations;
