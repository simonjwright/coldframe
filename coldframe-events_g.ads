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
--  $Revision: cc2d36d988d5 $
--  $Date: 2002/03/22 05:57:51 $
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

   ------------------
   --  Exceptions  --
   ------------------

   Cant_Happen : exception;
   --  Raised when an unexpected Event occurs.

   Use_Error : exception;
   --  Raised on misuse of the facilities (eg, attempting to post
   --  Events for the same Instance to more than one Queue; attempting
   --  to set a Timer that's already set).


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
   --  All Instance Events are derived from this
   --  type. For_The_Instance is the instance to which the event is
   --  directed.


   ---------------------
   --  Event queuing  --
   ---------------------

   type Event_Queue_Base is abstract tagged limited private;
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

   type Event_Queue_P is access all Event_Queue_Base'Class;

   procedure Post (The : Event_P;
                   On : access Event_Queue_Base) is abstract;
   --  The normal method of adding events to the event queue.
   --
   --  May raise Use_Error if the Event is an Instance_Event and
   --  Instance_Events for this Instance have previously been posted
   --  to a different Queue.


   --------------
   --  Timers  --
   --------------

   type Timer is limited private;
   --  Users declare these, in particular so that they can unset a
   --  timed event request when it is no longer needed (a timeout,
   --  perhaps, when the thing being timed out has actually occurred).


   procedure Set (The : in out Timer;
                  On : access Event_Queue_Base;
                  To_Fire : Event_P;
                  At_Time : Time.Time) is abstract;
   --  May raise Use_Error (if the Timer is already set)


   subtype Natural_Duration is Duration range 0.0 .. Duration'Last;

   procedure Set (The : in out Timer;
                  On : access Event_Queue_Base;
                  To_Fire : Event_P;
                  After : Natural_Duration) is abstract;
   --  May raise Use_Error (if the Timer is already set)

   procedure Unset (The : in out Timer;
                    On : access Event_Queue_Base) is abstract;
   --  May raise Use_Error (if the Timer is already unset)


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
   is abstract new Event_Base with null record;


   type Event_Queue_Base is abstract tagged limited null record;

   procedure Invalidate
     (On : access Event_Queue_Base;
      For_The_Instance : access Instance_Base'Class);


   --  Operations to support debug/logging. The implementation here
   --  is null.

   procedure Log_Retraction (The : Event_P;
                             On : access Event_Queue_Base);

   procedure Log_Pre_Dispatch (The : Event_P;
                               On : access Event_Queue_Base);

   procedure Log_Post_Dispatch (The : Event_P;
                                On : access Event_Queue_Base);


   type Timer_P is access all Timer;
   for Timer_P'Storage_Size use 0;

   --  A Timer_Event is dispatched like an ordinary event, but it has
   --  a special Handler which (after some checks) dispatches the held
   --  event.
   type Timer_Event is new Event_Base with record
      On : Event_Queue_P;
      Time_To_Fire : Time.Time;
      The_Event : Event_P;
      The_Timer : Timer_P;  -- null if the Timer has been deleted
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


end ColdFrame.Events_G;
