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

with Ada.Real_Time;
with ColdFrame.Events;
with Digital_IO.Initialize;
with Simple_Buttons.Initialize;
with Digital_IO.Arduino_Support;
with Simple_Buttons_Dispatcher;

procedure Simple_Buttons_Main is

   Environment_Task_Storage_Size : constant Natural := 4096
     with
       Export,
       Convention => Ada,
       External_Name => "_environment_task_storage_size";

   Dispatcher : constant ColdFrame.Events.Event_Queue_P
     := Simple_Buttons_Dispatcher.Dispatcher'Unchecked_Access;

begin

   Digital_IO.Initialize (Dispatcher);
   Digital_IO.Arduino_Support.Initialize;
   Simple_Buttons.Initialize (Dispatcher);

   delay until Ada.Real_Time.Time_Last;

end Simple_Buttons_Main;
