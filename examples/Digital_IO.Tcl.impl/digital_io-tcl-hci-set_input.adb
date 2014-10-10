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

--  Sets the specified input signal to the given value.

with Digital_IO.Input_Signal_State_Callback;
with Digital_IO.Tcl.Input_Cache;

separate (Digital_IO.Tcl.HCI)
procedure Set_Input
  (Of_Signal : Input_Signal;
   To : Boolean)
is
begin
   Input_Cache.Inputs (Of_Signal) := To;
   Input_Signal_State_Callback.Call_Callbacks ((S => Of_Signal, State => To));
end Set_Input;
