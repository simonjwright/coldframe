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

--  $RCSfile: normalize_xmi-model-operations.ads,v $
--  $Revision: 12a6c3b1d22b $
--  $Date: 2012/01/22 19:05:53 $
--  $Author: simonjwright $

private package Normalize_XMI.Model.Operations is

   function Read_Operation (From   : not null DOM.Core.Node;
                            Parent : not null Element_P) return Element_P;

private

   type Operation_Element is new Element with record
      Parameters : Element_Vectors.Vector;
      Return_Type : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   overriding
   procedure Resolve (O : in out Operation_Element);
   overriding
   procedure Output (O : Operation_Element; To : Ada.Text_IO.File_Type);

end Normalize_XMI.Model.Operations;
