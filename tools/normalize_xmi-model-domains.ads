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

--  $RCSfile: normalize_xmi-model-domains.ads,v $
--  $Revision: 8e850f2a9433 $
--  $Date: 2012/01/18 22:28:08 $
--  $Author: simonjwright $

with GNAT.OS_Lib;

package Normalize_XMI.Model.Domains is

   procedure Process_Domain (From : DOM.Core.Node; In_File : String);
   --  XXX not sure how to deal with <<interface>> subpackages.

private

   type Domain is new Element with record
      File_Time : GNAT.OS_Lib.OS_Time;
      Events : Element_Maps.Map;
      Classes : Element_Maps.Map;
      Types : Element_Maps.Map;
      Associations : Element_Maps.Map;
      Generalizations : Element_Maps.Map;
      Exceptions : Element_Maps.Map;
   end record;
   overriding
   function Find_Class (Known_To : Domain; Named : String) return Element_P;
   overriding
   procedure Resolve (D : in out Domain);
   overriding
   procedure Output (D : Domain; To : Ada.Text_IO.File_Type);

end Normalize_XMI.Model.Domains;
