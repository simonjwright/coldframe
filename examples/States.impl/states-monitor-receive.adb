--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  This is a class event handler.

--  It receives an external message indicating that a tick has
--  occurred for a Device. Generates a Heartbeat event for the Device
--  concerned.

with States.Events;
separate (States.Monitor)
procedure Receive
  (Tick_For : Device) is
begin
   ColdFrame.Project.Events.Post
     (The_Event => new Monitor.Heartbeat (Find ((Dev => Tick_For))),
      On => Events.Dispatcher);
end Receive;
