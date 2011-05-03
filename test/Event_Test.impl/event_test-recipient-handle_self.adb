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

with Event_Test.Events;

separate (Event_Test.Recipient)
procedure Handle_Self
  (P : Content) is
   E : constant ColdFrame.Project.Events.Event_P := new Mark (This);
begin
   This.Ordinal := P.Ordinal;
   Mark (E.all).Payload.Ordinal := This.Ordinal + 1;
   ColdFrame.Project.Events.Post (On => Events.Dispatcher, The_Event => E);
end Handle_Self;
