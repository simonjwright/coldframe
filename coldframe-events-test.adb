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

--  $RCSfile: coldframe-events-test.adb,v $
--  $Revision: 8e09f5990806 $
--  $Date: 2002/03/13 20:07:28 $
--  $Author: simon $

with Ada.Calendar;
with Ada.Task_Identification;
with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);
with ColdFrame.Events.Test_Support;
with GNAT.IO; use GNAT.IO;

procedure ColdFrame.Events.Test is

   Noisy_Ins : aliased Test_Support.Instance;
   Quiet_Ins : aliased Test_Support.Instance;

   E : Event_P;

begin

   E := new Test_Support.Noisy_Ev (Noisy_Ins'Unchecked_Access);
   Events.Set (The => Test_Support.T1,
               On => Test_Support.Noisy_Dispatcher,
               To_Fire => E,
               After => 2.0);

   E := new Test_Support.Noisy_Ev (Noisy_Ins'Unchecked_Access);
   Events.Set (The => Test_Support.T2,
               On => Test_Support.Noisy_Dispatcher,
               To_Fire => E,
               After => 1.0);

   delay 4.0;

   declare
      Start : Ada.Calendar.Time := Ada.Calendar.Clock;
      Interval : Duration;
      Ev : Event_P;
      use type Ada.Calendar.Time;
      Loops : constant := 10_000;
   begin
      for I in 1 .. Loops / 1000 loop
         for J in 1 .. 1000 loop
            Ev := new Test_Support.Quiet_Ev (Quiet_Ins'Unchecked_Access);
            Events.Post (The => Ev, On => Test_Support.Quiet_Dispatcher);
         end loop;
         delay 0.00001;
      end loop;
      Interval := ((Ada.Calendar.Clock - Start) / Loops) * 1_000_000;
      Put_Line ("wall dispatcher took" & Interval'Img);
   end;

   Ada.Task_Identification.Abort_Task (Ada.Task_Identification.Current_Task);

end ColdFrame.Events.Test;
