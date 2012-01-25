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

--  $RCSfile: normalize_xmi-model-types.adb,v $
--  $Revision: 7d1ad741f319 $
--  $Date: 2012/01/25 16:31:46 $
--  $Author: simonjwright $

package body Normalize_XMI.Model.Types is

   function Read_Type (From   : not null DOM.Core.Node;
                       Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new Type_Element;
   begin
      N.Parent := Parent;
      N.Name := +Read_Name (From_Element => From);
      return N;
   end Read_Type;


   not overriding
   function Type_Name (T : Type_Element) return String
   is
      Model_Name : constant String := +T.Name;
      Class : constant Element_P := T.Find_Class (Model_Name);
   begin
      if Class = null then
         return Model_Name;
      else
         return +Class.Name;
      end if;
   end Type_Name;

end Normalize_XMI.Model.Types;
