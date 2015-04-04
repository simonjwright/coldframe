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

with ColdFrame.Events;
with Digital_IO.Initialize;
with House_Management.Initialize;
with Digital_IO.STM32F4_Support;
with Stairwell_STM32F4_Demo_Dispatcher;
with Start_FreeRTOS_Scheduler;

procedure Stairwell_STM32F4_Demo is

   Dispatcher : constant ColdFrame.Events.Event_Queue_P
     := Stairwell_STM32F4_Demo_Dispatcher.Dispatcher'Unchecked_Access;

begin

   Digital_IO.Initialize (Dispatcher);
   Digital_IO.STM32F4_Support.Initialize;
   House_Management.Initialize (Dispatcher);

   Start_FreeRTOS_Scheduler;

end Stairwell_STM32F4_Demo;
