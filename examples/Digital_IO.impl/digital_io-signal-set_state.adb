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

--  $RCSfile: digital_io-signal-set_state.adb,v $
--  $Revision: e08cb16c3dfb $
--  $Date: 2014/03/11 18:27:45 $
--  $Author: simonjwright $

--  Set the signal's state as indicated; call Changed if it has
--  changed.

separate (Digital_IO.Signal)
procedure Set_State
  (This : not null Handle;
   To : Boolean := True) is
begin

   if To /= This.State then

      This.State := To;

      Changed (This);

   end if;

end Set_State;
