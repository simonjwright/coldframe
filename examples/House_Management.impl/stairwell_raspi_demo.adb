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

--  $Id: stairwell_raspi_demo.adb,v 20df44fbc8c2 2014/04/05 16:25:38 simonjwright $

with ColdFrame.Project.Events.Standard.Trace;
with Digital_IO.Initialize;
with Digital_IO.RasPi_Support;
with GNAT.Exception_Traces;
with House_Management.Initialize;

procedure Stairwell_RasPi_Demo is

   --  The ColdFrame event queue.
   Dispatcher : constant ColdFrame.Project.Events.Event_Queue_P
     := new ColdFrame.Project.Events.Standard.Trace.Event_Queue;

begin

   GNAT.Exception_Traces.Trace_On
     (Kind => GNAT.Exception_Traces.Unhandled_Raise);

   Digital_IO.Initialize (Dispatcher);
   Digital_IO.RasPi_Support.Initialize;
   House_Management.Initialize (Dispatcher);

end Stairwell_RasPi_Demo;
