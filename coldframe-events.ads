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

--  $RCSfile: coldframe-events.ads,v $
--  $Revision: 8937907f915d $
--  $Date: 2002/02/06 20:50:35 $
--  $Author: simon $

with Ada.Calendar;
with Ada.Real_Time;
with Ada.Finalization;
with ColdFrame.Instances;

package ColdFrame.States is

   pragma Elaborate_Body;

   -----------------------
   --  Class instances  --
   -----------------------

   type Instance_Base is abstract new Instances.Instance_Base with private;
   --  All Instances with state machines are derived from this type.

   function State_Image (This : Instance_Base) return String is abstract;
   --  Used for debugging/logging.


   --------------
   --  Events  --
   --------------

   type Event_Base (For_The_Instance : access Instance_Base'Class)
   is abstract tagged limited private;
   --  All Events are derived from this type. For_The_Instance is the
   --  instance to which the event is directed.

   type Event_P is access all Event_Base'Class;

   procedure Handler (This : Event_Base) is abstract;
   --  Concrete Events implement this to perform the required
   --  processing.


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

   type Timer is limited private;
   --  Users declare these, in particular so that they can unset a
   --  timed event request when it is no longer needed (a timeout,
   --  perhaps, when the thing being timed out has actually occurred).

   subtype Natural_Duration is Duration range 0.0 .. Duration'Last;

   procedure Set (The : in out Timer;
                  On : access Event_Queue_Base;
                  To_Fire : Event_P;
                  After : Natural_Duration) is abstract;
   --  May raise Use_Error (if the Timer is already set)

   procedure Unset (The : in out Timer;
                    On : access Event_Queue_Base) is abstract;
   --  May raise Use_Error (if the Timer is already unset)


   ------------------
   --  Exceptions  --
   ------------------

   Cant_Happen : exception;
   --  Raised when an unexpected Event occurs.

   Use_Error : exception;
   --  Raised on misuse of the state machine (eg, attempting to set a
   --  Timer that's already set).


private

   --  A Terminator is a component of an Instance_Base which is used
   --  to cause removal of any outstanding events for that instance
   --  from the scheduler when the instance is deleted.
   type Terminator (For_The_Instance : access Instance_Base)
   is new Ada.Finalization.Limited_Controlled with null record;

   procedure Finalize (The_Terminator : in out Terminator);


   type Instance_Base is abstract new Instances.Instance_Base with record
      The_Terminator : Terminator (Instance_Base'Access);
      Events_Posted_On : Event_Queue_P;
   end record;
   --  XXX Events_Posted_On is there for the Terminator to know which
   --  queue to retract events for this instance from. I guess it
   --  should be a constraint.


   type Event_Base (For_The_Instance : access Instance_Base'Class)
   is abstract tagged limited record
      Invalidated : Boolean := False;  --  set if the event is retracted
   end record;


   type Event_Queue_Base is abstract tagged limited null record;


   --  Operations to support debug/logging. The implementation here
   --  is null.

   procedure Log_Retraction (The : Event_P;
                             On : access Event_Queue_Base);

   procedure Log_Pre_Dispatch (The : Event_P;
                               On : access Event_Queue_Base);

   procedure Log_Post_Dispatch (The : Event_P;
                                On : access Event_Queue_Base);


   --  Timers: there is a complex race condition to be avoided.
   --
   --  Suppose:
   --
   --    there are two Timers set for the same state machine;
   --    the first fires and is posted to the event queue;
   --    the second fires;
   --    the first event's action unsets the second timer;
   --
   --  then the second timer will no longer be set here, and instead
   --  we have to remove the _event_ from the event queue (it must
   --  still be on the event queue, because we're in the context of an
   --  event action dispatched from that same queue).

   type State is (Initial, Set, Fired);

   type Timer is limited record
      The_Event : Event_P;
      Status : State := Initial;
      Real_Time_To_Fire : Ada.Real_Time.Time;
      Wall_Time_To_Fire : Ada.Calendar.Time;
   end record;
   --  The time at which the Timer is to fire may need to be
   --  Calendar.Time or Real_Time.Time, depending on the actual
   --  Timer_Queue; we supply both slots, Timer_Queue to choose.


end ColdFrame.States;
