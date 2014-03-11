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

--  $RCSfile: van_fleet-van-returned.adb,v $
--  $Revision: e08cb16c3dfb $
--  $Date: 2014/03/11 18:27:45 $
--  $Author: simonjwright $

with Van_Fleet.Pool_Van.Inheritance;
with Van_Fleet.Van.Inheritance;

separate (Van_Fleet.Van)
procedure Returned
  (This : not null Handle) is
   PVH : Pool_Van.Handle;
   pragma Warnings (Off, PVH);
begin
   if Get_G1_Child (This).Current /= Hired_Van_T then
      raise Use_Error;
   end if;
   Van.Inheritance.Delete_Child (This);
   PVH := Pool_Van.Inheritance.Create_Tree (ColdFrame.Instances.Handle (This));
end Returned;
