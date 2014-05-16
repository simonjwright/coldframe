--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with ColdFrame.Events_Test_Support;
with ColdFrame.Exceptions.Traceback;
with ColdFrame.Project.Events;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);

procedure ColdFrame.Events_Test is

   Noisy_Ins : aliased Events_Test_Support.Instance;

   E : Project.Events.Event_P;

begin

   E := new Events_Test_Support.Noisy_Ev (Noisy_Ins'Unchecked_Access);
   Project.Events.Set (The_Timer => Events_Test_Support.T1,
                       On => Events_Test_Support.Noisy_Dispatcher,
                       To_Fire => E,
                       After => 2.0);

   E := new Events_Test_Support.Noisy_Ev (Noisy_Ins'Unchecked_Access);
   Project.Events.Set (The_Timer => Events_Test_Support.T2,
                       On => Events_Test_Support.Noisy_Dispatcher,
                       To_Fire => E,
                       After => 1.0);

   delay 4.0;

   Project.Events.Tear_Down (Events_Test_Support.Noisy_Dispatcher);
   Project.Events.Tear_Down (Events_Test_Support.Quiet_Dispatcher);

end ColdFrame.Events_Test;
