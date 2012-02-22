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

with GNAT.IO; use GNAT.IO;
with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);
with ColdFrame.Project.Events.Standard.Trace;
with States.Initialize;
with States.Events;
with States.Monitor;
procedure States.T is
   E : ColdFrame.Project.Events.Event_P;
begin
   Initialize (new ColdFrame.Project.Events.Standard.Trace.Event_Queue);
   for N in 1 .. 10 loop
      delay 1.0;
      E := new Monitor.Tick;
      Monitor.Tick (E.all).Payload := Input;
      Put_Line ("generating input Tick");
      ColdFrame.Project.Events.Post (The_Event => E, On => Events.Dispatcher);
   end loop;
   Put_Line ("4 second gap in input Tick");
   delay 4.0;
   for N in 1 .. 10 loop
      delay 1.0;
      E := new Monitor.Tick;
      Monitor.Tick (E.all).Payload := Input;
      Put_Line ("generating input Tick");
      ColdFrame.Project.Events.Post (The_Event => E, On => Events.Dispatcher);
   end loop;
   Put_Line ("stopping input Tick");
end States.T;
