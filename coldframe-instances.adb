--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile: coldframe-instances.adb,v $
--  $Revision: b96d48dad1b3 $
--  $Date: 2003/07/13 06:33:51 $
--  $Author: simon $

package body ColdFrame.Instances is


   function Instance_Identifier_Equality
     (L, R : Instance_Base) return Boolean is
      pragma Warnings (Off, L);
      pragma Warnings (Off, R);
   begin
      raise Program_Error;
      return False;
   end Instance_Identifier_Equality;


   function Instance_Hash (Of_The_Instance : Instance_Base) return Natural is
      pragma Warnings (Off, Of_The_Instance);
   begin
      return 0;
   end Instance_Hash;


   function Classwide_Identifier_Equality (L, R : Handle) return Boolean is
   begin
      return Instance_Identifier_Equality (L.all, R.all);
   end Classwide_Identifier_Equality;


   function Classwide_Hash (Of_The_Handle : Handle) return Natural is
   begin
      return Instance_Hash (Of_The_Handle.all);
   end Classwide_Hash;


end ColdFrame.Instances;
