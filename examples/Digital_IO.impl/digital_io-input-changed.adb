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

--  $RCSfile: digital_io-input-changed.adb,v $
--  $Revision: e08cb16c3dfb $
--  $Date: 2014/03/11 18:27:45 $
--  $Author: simonjwright $

--  Called when the signal has changed state; notify any registered
--  observers of Signal State.

with Digital_IO.Signal_State_Callback;

separate (Digital_IO.Input)
procedure Changed
  (This : not null Handle) is
begin

   Signal_State_Callback.Call_Callbacks
     (With_Param => (S => Get_S (This),
                     State => Get_State (This)));

end Changed;
