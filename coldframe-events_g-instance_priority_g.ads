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

--  $RCSfile: coldframe-events_g-instance_priority_g.ads,v $
--  $Revision: fbc368af748a $
--  $Date: 2006/07/17 04:44:54 $
--  $Author: simonjwright $

with Ada.Task_Identification;
with BC.Containers.Queues.Unbounded;
with ColdFrame.Events_G.Held_Event_Queue_Signature;

generic

   with package Held_Events is new Held_Event_Queue_Signature (<>);

package ColdFrame.Events_G.Instance_Priority_G is

   pragma Elaborate_Body;

   ---------------------
   --  Event queuing  --
   ---------------------

   type Event_Queue_Base (Start_Started : Boolean;
                          Priority : System.Priority;
                          Storage_Size : Positive)
   is new Events_G.Event_Queue_Base with private;

   subtype Event_Queue is Event_Queue_Base
     (Start_Started => True,
      Priority => System.Default_Priority,
      Storage_Size => 20_000);

   procedure Post (The_Event : Event_P;
                   On : access Event_Queue_Base);

   procedure Post_To_Self (The_Event : Event_P;
                           On : access Event_Queue_Base);

   ----------------------
   --  Delayed events  --
   ----------------------

   procedure Post (The_Event : Event_P;
                   On : access Event_Queue_Base;
                   To_Fire_At : Time.Time);

   procedure Post (The_Event : Event_P;
                   On : access Event_Queue_Base;
                   To_Fire_After : Natural_Duration);

   --------------
   --  Timers  --
   --------------

   procedure Set (The_Timer : in out Timer;
                  On : access Event_Queue_Base;
                  To_Fire : Event_P;
                  At_Time : Time.Time);

   procedure Set (The_Timer : in out Timer;
                  On : access Event_Queue_Base;
                  To_Fire : Event_P;
                  After : Natural_Duration);

   procedure Unset (The_Timer : in out Timer;
                    On : access Event_Queue_Base);

private

   package Abstract_Posted_Event_Containers
   is new BC.Containers (Event_P);
   package Abstract_Posted_Event_Queues
   is new Abstract_Posted_Event_Containers.Queues;
   package Unbounded_Posted_Event_Queues
   is new Abstract_Posted_Event_Queues.Unbounded
     (Storage => Event_Storage);


   task type Dispatcher (The_Queue : access Event_Queue_Base'Class;
                         Priority : System.Priority;
                         Storage_Size : Positive) is

      --  We need to constrain by 'Class so that internal calls to
      --  potentially dispatching operations (such as
      --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

      pragma Task_Name ("aDispatcher");
      pragma Priority (Priority);
      pragma Storage_Size (Storage_Size);


      entry Start;

      entry Finish;
      --  Called during tear down, in case the Queue hasn't been started.

   end Dispatcher;


   task type Held_Event_Manager (The_Queue : access Event_Queue_Base'Class) is

      pragma Task_Name ("aHeldEventManager");
      --  No need to specify priority (because we only deal with timed
      --  events anyway) or stack size (if anything, we could reduce
      --  it, since there's no user code to call).

      --  We need to constrain by 'Class so that internal calls to
      --  potentially dispatching operations (such as
      --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

      entry Add_At_Event (The_Entry : Event_P;
                          To_Run_At : Time.Time);

      entry Add_After_Event (The_Entry : Event_P;
                             To_Run_After : Duration);

      entry Rethink;
      --  Something has happened which may alter the relative timings
      --  (or, the queue has been started so that "after" events are
      --  now available)

      entry Invalidate (For_The_Instance : Instance_Base_P);
      --  Marks all the events on the queue which are for
      --  For_The_Instance as invalid, so they won't be actioned when
      --  their time arrives.

      entry Stop;
      --  Only for use by domain Tear_Down.

      entry Stopped;
      --  Only for use by domain Tear_Down. Indicates readiness to be
      --  aborted.

      entry Finish;
      --  Only for use by domain Tear_Down. End execution.

   end Held_Event_Manager;


   --  Mutual exclusion between posters and the Dispatcher.
   protected type Excluder (The_Queue : access Event_Queue_Base'Class) is

      --  We need to constrain by 'Class so that internal calls to
      --  potentially dispatching operations (such as
      --  Log_{Pre,Post}_Dispatch) will in fact dispatch.

      entry Post (The_Event : Event_P);
      --  Post an event.

      entry Post_To_Self (The_Event : Event_P);
      --  Post an event-to-self.

      entry Fetch (The_Event : out Event_P; Stopping : out Boolean);
      --  Blocks until the queue is unlocked and there is an event on
      --  it; when one is found, notes that execution is in progress,
      --  removes the event from the queue and stores it
      --  in "The_Event".
      --
      --  If the queue is being stopped, however, sets Stopping
      --  to True.

      procedure Invalidate_Events
        (For_The_Instance : access Instance_Base'Class);
      --  Marks all the events on the queue which are for
      --  For_The_Instance as invalid, so they won't be actioned when
      --  Fetched.

      entry Lock;
      --  Blocks until execution isn't in progress.

      procedure Unlock;
      --  Notes that the queue is no longer locked.

      procedure Done;
      --  Notes that execution is no longer in progress.

      procedure Stop;
      --  Only for use by domain Tear_Down.

   private

      entry Waiting_For_Lock;
      --  Requeued by Lock if the calling task isn't already the
      --  Owner.

      Locks : Natural := 0;
      --  Set to 1 while handling an event or a first lock has been taken.
      --  Incremented past 1 when a task which already has the lock takes it
      --  again (POSIX "recursive mutex" behaviour).

      Owner : Ada.Task_Identification.Task_Id;
      --  Supports "recursive mutex" behaviour, and checks that
      --  Post_To_Self is called during event processing, not by some
      --  external task.

      Stopping : Boolean := False;

   end Excluder;


   type Event_Queue_Base (Start_Started : Boolean;
                          Priority : System.Priority;
                          Storage_Size : Positive)
   is new Events_G.Event_Queue_Base (Start_Started => Start_Started,
                                     Priority => Priority,
                                     Storage_Size => Storage_Size)
   with record
      The_Excluder : Excluder (Event_Queue_Base'Access);
      The_Self_Events : Unbounded_Posted_Event_Queues.Queue;
      The_Instance_Events : Unbounded_Posted_Event_Queues.Queue;
      The_Class_Events : Unbounded_Posted_Event_Queues.Queue;
      The_Held_Events : aliased Held_Events.Queue;
      The_Dispatcher : Dispatcher (Event_Queue_Base'Access,
                                   Priority => Priority,
                                   Storage_Size => Storage_Size);
      The_Held_Event_Manager : Held_Event_Manager (Event_Queue_Base'Access);
   end record;

   procedure Start_Queue (The_Queue : access Event_Queue_Base);

   procedure Invalidate_Events
     (On : access Event_Queue_Base;
      For_The_Instance : access Instance_Base'Class);

   procedure Stop (The_Queue : in out Event_Queue_Base);

   procedure Tear_Down (The_Queue : in out Event_Queue_Base);

   --  Locking.

   procedure Locker (The_Queue : access Event_Queue_Base);

   procedure Unlocker (The_Queue : access Event_Queue_Base);

end ColdFrame.Events_G.Instance_Priority_G;