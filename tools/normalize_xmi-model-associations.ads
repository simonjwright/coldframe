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

--  $RCSfile: normalize_xmi-model-associations.ads,v $
--  $Revision: 113b7da65bbd $
--  $Date: 2011/12/20 21:01:07 $
--  $Author: simonjwright $

private package Normalize_XMI.Model.Associations is

   function Read_Association (From : DOM.Core.Node;
                              Parent : not null Element_P) return Element_P;

private

   type Association_Element is new Element with record
      Ends : Element_Vectors.Vector;
   end record;
   overriding
   procedure Resolve (A : in out Association_Element);
   overriding
   procedure Output (A : Association_Element; To : Ada.Text_IO.File_Type);

end Normalize_XMI.Model.Associations;
