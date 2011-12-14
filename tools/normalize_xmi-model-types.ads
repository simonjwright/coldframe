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

--  $RCSfile: normalize_xmi-model-types.ads,v $
--  $Revision: 8e07091e803e $
--  $Date: 2011/12/14 21:19:23 $
--  $Author: simonjwright $

private package Normalize_XMI.Model.Types is

   function Read_Type (From : DOM.Core.Node) return Element_P;

private

   type Type_Element is new Element with record
      Attributes_Permitted : Boolean := False;
      Attributes : Element_Maps.Map;
      Operations : Element_Maps.Map;
   end record;
   overriding
   procedure Resolve (T : in out Type_Element);
   overriding
   procedure Output (T : Type_Element; To : Ada.Text_IO.File_Type);

end Normalize_XMI.Model.Types;
