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

--  $RCSfile: house_management-button-pushed.adb,v $
--  $Revision: 0f7eda971a8e $
--  $Date: 2003/02/07 05:55:57 $
--  $Author: simon $

--  The button has been pushed; tell the associated Lamp(s).

with House_Management.Lamp.Collections;
with House_Management.Lamp.Iterate;
with House_Management.A1;

separate (House_Management.Button)
procedure Pushed
  (This : Handle) is

   procedure Process is new Lamp.Iterate (Lamp.Button_Pushed);

   LHs : constant Lamp.Collections.Collection
     := A1.Is_Controlled_By (This);

begin

   Process (LHs);

end Pushed;
