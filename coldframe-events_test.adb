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

--  $RCSfile: coldframe-events_test.adb,v $
--  $Revision: 281d11e491da $
--  $Date: 2002/07/27 13:05:23 $
--  $Author: simon $

with Ada.Calendar;
with Ada.Task_Identification;
with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);
with ColdFrame.Events_Test_Support;
with ColdFrame.Project.Events;
with GNAT.IO; use GNAT.IO;

procedure ColdFrame.Events_Test is

   Noisy_Ins : aliased Events_Test_Support.Instance;
   Quiet_Ins : aliased Events_Test_Support.Instance;

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

   declare
      Start : Ada.Calendar.Time := Ada.Calendar.Clock;
      Interval : Duration;
      Ev : Project.Events.Event_P;
      use type Ada.Calendar.Time;
      Loops : constant := 10_000;
   begin
      for I in 1 .. Loops / 1000 loop
         for J in 1 .. 1000 loop
            Ev := new Events_Test_Support.Quiet_Ev
              (Quiet_Ins'Unchecked_Access);
            Project.Events.Post (The_Event => Ev,
                                 On => Events_Test_Support.Quiet_Dispatcher);
         end loop;
         delay 0.00001;
      end loop;
      Interval := ((Ada.Calendar.Clock - Start) / Loops) * 1_000_000;
      Put_Line ("wall dispatcher took" & Interval'Img);
   end;

   Ada.Task_Identification.Abort_Task (Ada.Task_Identification.Current_Task);

end ColdFrame.Events_Test;
