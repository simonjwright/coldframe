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

--  $RCSfile: coldframe-events_g-standard_g.ads,v $
--  $Revision: f0250ccf39c6 $
--  $Date: 2002/07/16 17:37:03 $
--  $Author: simon $

with BC.Containers.Queues.Unbounded;
with BC.Containers.Queues.Ordered.Unbounded;

generic
package ColdFrame.Events_G.Standard_G is

   pragma Elaborate_Body;

   ---------------------
   --  Event queuing  --
   ---------------------

   type Event_Queue is new Event_Queue_Base with private;

   procedure Post (The_Event : Event_P;
                   On : access Event_Queue);

   procedure Post_To_Self (The_Event : Event_P;
                           On : access Event_Queue);

   ----------------------
   --  Delayed events  --
   ----------------------

   procedure Post (The_Event : Event_P;
                   On : access Event_Queue;
                   To_Fire_At : Time.Time);

   procedure Post (The_Event : Event_P;
                   On : access Event_Queue;
                   To_Fire_After : Natural_Duration);

   --------------
   --  Timers  --
   --------------

   procedure Set (The_Timer : in out Timer;
                  On : access Event_Queue;
                  To_Fire : Event_P;
                  At_Time : Time.Time);

   procedure Set (The_Timer : in out Timer;
                  On : access Event_Queue;
                  To_Fire : Event_P;
                  After : Natural_Duration);

   procedure Unset (The_Timer : in out Timer;
                    On : access Event_Queue);

   -------------------------
   --  Unit test support  --
   -------------------------

   procedure Wait_Until_Idle (The_Queue : access Event_Queue);

private

   package Abstract_Posted_Event_Containers
   is new BC.Containers (Event_P);
   package Abstract_Posted_Event_Queues
   is new Abstract_Posted_Event_Containers.Queues;
   package Unbounded_Posted_Event_Queues
   is new Abstract_Posted_Event_Queues.Unbounded
     (Storage => Event_Storage);

   function "<" (L, R : Timer_Queue_Entry_P) return Boolean;

   package Abstract_Timed_Event_Containers
   is new BC.Containers (Timer_Queue_Entry_P);
   package Abstract_Timed_Event_Queues
   is new Abstract_Timed_Event_Containers.Queues;
   package Abstract_Timed_Event_Ordered_Queues
   is new Abstract_Timed_Event_Queues.Ordered;
   package Timed_Event_Queues
   is new Abstract_Timed_Event_Ordered_Queues.Unbounded
     (Storage => Event_Storage);


   task type Dispatcher (The_Queue : access Event_Queue'Class) is

      --  We need to constrain by 'Class so that internal calls to
      --  potentially dispatching operations (such as
      --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

   end Dispatcher;


   task type Timer_Manager (The_Queue : access Event_Queue'Class) is

      --  We need to constrain by 'Class so that internal calls to
      --  potentially dispatching operations (such as
      --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

      entry Append (The_Entry : Timer_Queue_Entry_P);

      entry Invalidate (For_The_Instance : Instance_Base_P);
      --  Marks all the events on the queue which are for
      --  For_The_Instance as invalid, so they won't be actioned when
      --  Fetched.

   end Timer_Manager;


   --  Mutual exclusion between posters and the Dispatcher.
   protected type Excluder (The_Queue : access Event_Queue'Class) is

      --  We need to constrain by 'Class so that internal calls to
      --  potentially dispatching operations (such as
      --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

      procedure Post (The_Event : Event_P);
      --  Post an event.

      procedure Post_To_Self (The_Event : Event_P);
      --  Post an event-to-self.

      entry Fetch (The_Event : out Event_P);
      --  Blocks until the queue is unlocked and there is an event on
      --  it; when one is found, notes that execution is in progress,
      --  removes the event from the queue and stores it
      --  in"The_Event".

      procedure Done;
      --  Notes that execution is no longer in progress.

      procedure Invalidate_Events
        (For_The_Instance : access Instance_Base'Class);
      --  Marks all the events on the queue which are for
      --  For_The_Instance as invalid, so they won't be actioned when
      --  Fetched.

      entry Lock;
      --  Blocks until execution isn't in progress.

      procedure Unlock;
      --  Notes that the queue is no longer locked.

   private

      Locked : Boolean := False;
      Executing : Boolean := False;

   end Excluder;


   --  Determining whether there are any events left (if not, unit test can
   --  stop).
   protected type Event_Count is

      entry Wait_Until_Idle;
      --  Blocks until there are no events pending or held.

      procedure Add_Posted_Event;
      procedure Remove_Posted_Event;
      procedure Add_Held_Event;
      procedure Remove_Held_Event;

   private

      Posted_Events : Natural := 0;
      Held_Events : Natural := 0;

   end Event_Count;


   --  The actual Event Queue.
   type Event_Queue is new Event_Queue_Base with record
      The_Excluder : Excluder (Event_Queue'Access);
      The_Self_Events : Unbounded_Posted_Event_Queues.Queue;
      The_Events : Unbounded_Posted_Event_Queues.Queue;
      The_Dispatcher : Dispatcher (Event_Queue'Access);
      The_Timer_Manager : Timer_Manager (Event_Queue'Access);
      The_Event_Count : Event_Count;
   end record;

   procedure Invalidate_Events
     (On : access Event_Queue;
      For_The_Instance : access Instance_Base'Class);

   procedure Tear_Down (The_Queue : in out Event_Queue);

   --  Locking.

   procedure Locker (The_Queue : access Event_Queue);

   procedure Unlocker (The_Queue : access Event_Queue);

end ColdFrame.Events_G.Standard_G;
