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

--  $RCSfile: coldframe-events_test_support.ads,v $
--  $Revision: e961833ca95d $
--  $Date: 2002/09/18 18:43:37 $
--  $Author: simon $

with ColdFrame.Project;
with ColdFrame.Project.Events.Standard.Debug;

package ColdFrame.Events_Test_Support is

   type Instance is new Project.Events.Instance_Base with null record;
   function State_Image (This : Instance) return String;

   type Quiet_Ev (For_The_Instance : access Project.Events.Instance_Base'Class)
      is new Project.Events.Instance_Event_Base (For_The_Instance)
     with null record;

   procedure Handler (This : Quiet_Ev);

   type Noisy_Ev (For_The_Instance : access Project.Events.Instance_Base'Class)
      is new Project.Events.Instance_Event_Base (For_The_Instance)
     with null record;

   procedure Handler (This : Noisy_Ev);

   Quiet_Dispatcher : Project.Events.Event_Queue_P
     := new Project.Events.Standard.Event_Queue;
   Noisy_Dispatcher : Project.Events.Event_Queue_P
     := new Project.Events.Standard.Debug.Event_Queue (Start_Started => True);

   T1, T2 : Project.Events.Timer;

end ColdFrame.Events_Test_Support;
