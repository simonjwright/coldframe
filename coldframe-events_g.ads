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

--  $RCSfile: coldframe-events_g.ads,v $
--  $Revision: 7f8de3dc1e88 $
--  $Date: 2002/09/15 10:31:13 $
--  $Author: simon $

with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with ColdFrame.Instances;
with ColdFrame.Time_Signature;
with System.Storage_Pools;

generic

   with package Time is new Time_Signature (<>);

   Event_Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;

package ColdFrame.Events_G is

   pragma Elaborate_Body;

   -----------------------
   --  Class instances  --
   -----------------------

   type Instance_Base is abstract new Instances.Instance_Base with private;
   --  All Instances with state machines are derived from this type.

   type Instance_Base_P is access all Instance_Base;
   --  We need to convert "access Instance_Base" to something
   --  compare-able. GNAT 3.15a (Linux) generates slightly less code
   --  if we declare here rather than locally below.
   for Instance_Base_P'Storage_Size use 0;
   --  .. but, of course, no storage collection is required.

   function State_Image (This : Instance_Base) return String is abstract;
   --  Used for debugging/logging.


   --------------
   --  Events  --
   --------------

   type Event_Base is abstract tagged limited private;
   --  All Events are derived from this type.

   type Event_P is access all Event_Base'Class;
   for Event_P'Storage_Pool use Event_Storage;


   procedure Handler (This : Event_Base) is abstract;
   --  Concrete Events implement this to perform the required
   --  processing.

   type Instance_Event_Base (For_The_Instance : access Instance_Base'Class)
   is abstract new Event_Base with private;
   --  All Instance Events are derived from this type. For_The_Instance
   --  is the instance to which the event is directed.

   procedure Instance_Is_Deleted
     (For_The_Event : access Instance_Event_Base'Class);
   --  Note that the Instance has been deleted (so as to avoid
   --  querying it in logging Queue variants).


   ---------------------
   --  Event queuing  --
   ---------------------

   type Event_Queue_Base (Start_Started : Boolean)
   is abstract tagged limited private;
   --  An Event Queue is intended to decouple the occurrence of an
   --  event from handling it. Normally, each Domain would have one
   --  Event Queue; events are posted to the Queue by a number of
   --  tasks (typically by service Domains) and handled by a single
   --  task in the owning Domain. At the other extreme, one could have
   --  one Queue for the whole application.
   --
   --  Given the decoupling motivation, there's no particular reason
   --  why Events should always be handled by state machines.
   --
   --  Different Event Queue implementations can have different
   --  strategies (eg, priority queuing, logging).
   --
   --  if Start_Started is False, call Start to begin processing
   --  Events (which can be Posted or Set on the Queue beforehand).

   type Event_Queue_P is access all Event_Queue_Base'Class;

   procedure Start (The_Queue : access Event_Queue_Base);
   --  Raises Use_Error if the Queue is already started.

   procedure Post (The_Event : Event_P;
                   On : access Event_Queue_Base) is abstract;
   --  The normal method of adding events to the event queue.
   --
   --  May raise Exceptions.Use_Error if the Event is an
   --  Instance_Event and Instance_Events for this Instance have
   --  previously been posted to a different Queue.

   procedure Post_To_Self (The_Event : Event_P;
                           On : access Event_Queue_Base) is abstract;
   --  Events to self take precedence over externally- or
   --  timer-generated events.


   ----------------------
   --  Delayed events  --
   ----------------------

   --  These two interfaces are to be used in "fire and forget" mode.
   --  Timers (below) allow users to retract events (for example, when
   --  you set a timeout for some occurrence and the occurrence
   --  actually occurs).
   --
   --  Use these interfaces if there's no need for retraction (a
   --  regular heartbeat, perhaps) or if you need lots of events
   --  queued up (a queue of scenario events).

   subtype Natural_Duration is Duration range 0.0 .. Duration'Last;

   procedure Post (The_Event : Event_P;
                   On : access Event_Queue_Base;
                   To_Fire_At : Time.Time) is abstract;

   procedure Post (The_Event : Event_P;
                   On : access Event_Queue_Base;
                   To_Fire_After : Natural_Duration) is abstract;


   --------------
   --  Timers  --
   --------------

   type Timer is limited private;
   --  Users declare these, in particular so that they can unset a
   --  timed event request when it is no longer needed (a timeout,
   --  perhaps, when the thing being timed out has actually occurred).


   procedure Set (The_Timer : in out Timer;
                  On : access Event_Queue_Base;
                  To_Fire : Event_P;
                  At_Time : Time.Time) is abstract;
   --  May raise Exceptions.Use_Error (if the Timer is already set)

   procedure Set (The_Timer : in out Timer;
                  On : access Event_Queue_Base;
                  To_Fire : Event_P;
                  After : Natural_Duration) is abstract;
   --  May raise Exceptions.Use_Error (if the Timer is already set)

   procedure Unset (The_Timer : in out Timer;
                    On : access Event_Queue_Base) is abstract;
   --  May raise Exceptions.Use_Error (if the Timer is already unset)


   ---------------
   --  Locking  --
   ---------------

   --  This type is typically to be used by <<public>> operations, to
   --  ensure mutual exclusion between them and dispatched Events.
   --
   --  The "resource acquisition is initialization" idiom is used:
   --
   --     procedure P is
   --        Lock : ColdFrame.Project.Events.Lock (Domain.Events.Dispatcher);
   --        pragma Warnings (Off, Lock);
   --     begin
   --        ... the Dispatcher is released when the procedure exits

   type Lock (The_Queue : access Event_Queue_Base'Class) is limited private;


   -------------------------
   --  Unit test support  --
   -------------------------

   --  The implementations here raise Exceptions.Use_Error. Must only
   --  be used with an instantiation of Events_G.Test_G.

   procedure Wait_Until_Idle (The_Queue : access Event_Queue_Base;
                              Ignoring_Timers : Boolean := False);
   --  Blocks the caller until there are no more events awaiting
   --  processing.
   --  If Ignoring_Timers is True, ignores events held on Timers; this
   --  feature is intended for the case where a unit test has driven
   --  the state machine to a point where a timer is set, but need go
   --  no further.

   procedure Tear_Down (The_Queue : in out Event_Queue_P);
   --  Terminates any tasks and deallocates The_Queue.


private

   --  Event management (plain and timed) is fairly straightforward
   --  until we consider deletion of instances and unsetting timers.
   --
   --  If an instance is deleted then any events directed to it must
   --  not be actioned.
   --
   --  If a timer is deleted (which will happen when an instance with
   --  a timer as an instance variable is deleted) then any pending
   --  event held in it must not be actioned. Note it's possible
   --  (though dubious) for the held event to be directed to a
   --  different instance (possibly of a different class) from that
   --  holding the timer.
   --
   --  If a timer is unset the held event must not be actioned.
   --
   --  Memory must be freed when finished with.


   --  An Instance_Terminator is a component of an Instance_Base which
   --  is used to cause removal of any outstanding events for that
   --  instance from the scheduler when the instance is deleted.
   type Instance_Terminator (For_The_Instance : access Instance_Base)
   is new Ada.Finalization.Limited_Controlled with null record;

   procedure Finalize (The_Terminator : in out Instance_Terminator);


   type Instance_Base is abstract new Instances.Instance_Base with record
      The_Terminator : Instance_Terminator (Instance_Base'Access);
      Events_Posted_On : Event_Queue_P;
   end record;
   --  Events_Posted_On is there for the Instance_Terminator to know which
   --  queue to retract events for this instance from.


   type Event_Base is abstract tagged limited record
      Invalidated : Boolean := False;  --  set if the event is retracted
   end record;

   procedure Delete
   is new Ada.Unchecked_Deallocation (Event_Base'Class, Event_P);


   type Instance_Event_Base (For_The_Instance : access Instance_Base'Class)
   is abstract new Event_Base with record
      Instance_Deleted : Boolean := False;
   end record;


   type Event_Queue_Base (Start_Started : Boolean)
   is abstract tagged limited record
         Started : Boolean := Start_Started;
   end record;


   --  Default private interface to invalidate events to deleted
   --  instances. The implementation here raises Program_Error if
   --  called.
   procedure Invalidate_Events
     (On : access Event_Queue_Base;
      For_The_Instance : access Instance_Base'Class);

   --  Default private interface to tear down an event queue.  The
   --  implementation here raises Exceptions.Use_Error if called.
   procedure Tear_Down (The_Queue : in out Event_Queue_Base);

   --  Operations to support test.

   --  Operations to support starting.

   procedure Start_Queue (The_Queue : access Event_Queue_Base);
   --  Raises Program_Error.

   --  Operations to support Wait_Until_Idle. The implementations here
   --  are null.

   procedure Add_Posted_Event (On : access Event_Queue_Base);

   procedure Remove_Posted_Event (On : access Event_Queue_Base);

   procedure Add_Held_Event (On : access Event_Queue_Base);

   procedure Remove_Held_Event (On : access Event_Queue_Base);

   procedure Add_Timer_Event (On : access Event_Queue_Base);

   procedure Remove_Timer_Event (On : access Event_Queue_Base);

   --  Operations to support debug/logging. The implementations here
   --  are null.

   procedure Log_Retraction (The_Event : Event_P;
                             On : access Event_Queue_Base);

   procedure Log_Pre_Dispatch (The_Event : Event_P;
                               On : access Event_Queue_Base);

   procedure Log_Post_Dispatch (The_Event : Event_P;
                                On : access Event_Queue_Base);

   --  Operations to support Locking. The implementations here raise
   --  Program_Error if called.

   procedure Locker (The_Queue : access Event_Queue_Base);

   procedure Unlocker (The_Queue : access Event_Queue_Base);


   type Timer_P is access all Timer;
   for Timer_P'Storage_Size use 0;

   --  A Timer_Event is dispatched like an ordinary event, but it has
   --  a special Handler which (after some checks) dispatches the held
   --  event.
   --
   --  This is so that we can tell when the event is actually dispatched,
   --  in case the user Unsets it.
   type Timer_Event is new Event_Base with record
      On : Event_Queue_P;
      Time_To_Fire : Time.Time;
      The_Event : Event_P;
      The_Timer : Timer_P;  -- null if the Timer has been deleted, or no Timer
   end record;

   procedure Handler (This : Timer_Event);


   type Timer_Queue_Entry_P is access Timer_Event;
   for Timer_Queue_Entry_P'Storage_Pool use Event_Storage;


   --  A Timer_Terminator is a component of a Timer which is used to
   --  cause removal of any outstanding events for that timer from the
   --  scheduler when the (instance containing) the timer is deleted.
   --
   --  We have to avoid making Timer tagged so as to avoid trying to
   --  dispatch on more than one parameter.

   type Timer_Terminator (For_The_Timer : access Timer)
   is new Ada.Finalization.Limited_Controlled with null record;

   procedure Finalize (The_Terminator : in out Timer_Terminator);


   type Timer is limited record
      The_Terminator : Timer_Terminator (Timer'Access);
      The_Entry : Timer_Queue_Entry_P;
   end record;


   type Lock (The_Queue : access Event_Queue_Base'Class)
      is new Ada.Finalization.Limited_Controlled with record
        Finalized : Boolean := False;
      end record;

   procedure Initialize (The_Lock : in out Lock);
   procedure Finalize (The_Lock : in out Lock);


end ColdFrame.Events_G;
