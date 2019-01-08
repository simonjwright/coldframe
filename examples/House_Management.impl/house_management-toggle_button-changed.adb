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

--  This overriding implementation is called to indicate that the
--  Button has been pushed or released.

--  For this kind of Button, toggles On in case of a push; a release
--  is ignored.

--  Notifies connected lamps of the change.

separate (House_Management.Toggle_Button)
procedure Changed
  (This : not null Handle;
   Pushed : Boolean) is
begin
   if Pushed then
      This.On := not This.On;
   end if;
   Notify_Connected_Lamps (This);
end Changed;
