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

--  $RCSfile: digital_io-hci-get_state.adb,v $
--  $Revision: 25594980cdc0 $
--  $Date: 2003/02/07 05:51:00 $
--  $Author: simon $

--  Returns the state of the specified signal.

with Digital_IO.Signal;

separate (Digital_IO.HCI)
function Get_State
  (Of_Signal : Signal_Name)
  return Boolean is
begin

   return Signal.Get_State (Signal.Find ((S => Of_Signal)));

end Get_State;
