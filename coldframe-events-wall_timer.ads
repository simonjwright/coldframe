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

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile: coldframe-events-wall_timer.ads,v $
--  $Revision: f880bcb9ecd3 $
--  $Date: 2002/02/01 20:48:43 $
--  $Author: simon $

with BC.Containers.Queues.Unbounded;
with BC.Containers.Queues.Synchronized;
with BC.Support.Synchronization;
with ColdFrame.Global_Storage_Pool;

package ColdFrame.States.Wall_Timer is

   pragma Elaborate_Body;

   ---------------------
   --  Event queuing  --
   ---------------------

   type Event_Queue is new Event_Queue_Base with private;

   procedure Post (It : Event_Base'Class;
                   On : access Event_Queue);

   procedure Set (The : in out Timer;
                  On : access Event_Queue;
                  To_Fire : access Event_Base'Class;
                  After : Natural_Duration);

   procedure Unset (The : in out Timer;
                    On : access Event_Queue);

private

   --  We're going to use a Synchronized Queue because there are two
   --  tasks interested in updating it: our Dispatcher, and the timer
   --  manager.

   package Abstract_Event_Containers
   is new BC.Containers (Event_P);
   package Abstract_Event_Queues
   is new Abstract_Event_Containers.Queues;

   package Unbounded_Event_Queues
   is new Abstract_Event_Queues.Unbounded
     (Storage => ColdFrame.Global_Storage_Pool.Pool);
   package Event_Queues is new Abstract_Event_Queues.Synchronized
     (Queue_Base => Unbounded_Event_Queues.Queue,
      Monitor => BC.Support.Synchronization.Single_Monitor);

   task type Dispatcher (The_Queue : access Event_Queue'Class);
   --  We need to constrain by 'Class so that internal calls to
   --  potentially dispatching operations (such as
   --  Log_{Pre,Post}_Dispatch) will in fact dispatch.


   type Timer_P is access all Timer;

   task type Timer_Manager (The_Queue : access Event_Queue'Class) is
      entry Append (The : Timer_P);
      entry Remove (The : Timer_P);
   end Timer_Manager;
   --  We need to constrain by 'Class so that internal calls to
   --  potentially dispatching operations (such as
   --  Log_{Pre,Post}_Dispatch) will in fact dispatch.


   type Event_Queue is new Event_Queue_Base with record
      The_Events : Event_Queues.Queue;
      The_Dispatcher : Dispatcher (Event_Queue'Access);
      The_Timer_Manager : Timer_Manager (Event_Queue'Access);
   end record;

   procedure Retract (It : Event_Base'Class;
                      On : access Event_Queue;
                      Success : out Boolean);
   --  Removes an event from the event queue; intended for use when we
   --  find out that a timed event has already fired when an action
   --  attempts to unset it.
   --  Doesn't deallocate the Event.
   --  If the event isn't in the event queue, Success is set False
   --  (this is probably a Use_Error).


   --  Operations to support debug/logging. The implementation here
   --  is null.

   procedure Log_Retraction (It : Event_Base'Class;
                             On : access Event_Queue);

   procedure Log_Pre_Dispatch (It : Event_Base'Class;
                               On : access Event_Queue);

   procedure Log_Post_Dispatch (It : Event_Base'Class;
                                On : access Event_Queue);

end ColdFrame.States.Wall_Timer;
