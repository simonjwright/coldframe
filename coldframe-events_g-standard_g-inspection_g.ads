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

--  $RCSfile: coldframe-events_g-standard_g-inspection_g.ads,v $
--  $Revision: d66face9dfd2 $
--  $Date: 2006/03/06 20:12:32 $
--  $Author: simonjwright $

with ColdFrame.Events_G.Held_Event_Queue_Signature.Inspection_Signature;

generic

   --  Enable queue inspection.
   with package Held_Events_Inspection
      is new Held_Events.Inspection_Signature (<>);

package ColdFrame.Events_G.Standard_G.Inspection_G is

   --  This package allows you to see what's on an Event Queue, so you
   --  don't need to test event posting just by seeing the
   --  aftereffects.
   --
   --  The queue must not have been started.

   Started : exception;
   --  Raised if the queue has been started.

   Not_Found : exception;
   --  Raised if you ask for an event that wasn't posted.

   --------------------------------
   --  Events posted to "self".  --
   --------------------------------

   --  How many?
   function Number_Of_Self_Events (On : Event_Queue_P) return Natural;
   --  Return the At_Index'th event (in order of posting).
   function Self_Event (On : Event_Queue_P;
                        At_Index : Positive) return Event_P;

   -----------------------------------------
   --  Events posted to run immediately.  --
   -----------------------------------------

   --  How many?
   function Number_Of_Now_Events (On : Event_Queue_P) return Natural;
   --  Return the At_Index'th event (in order of posting).
   function Now_Event (On : Event_Queue_P;
                       At_Index : Positive) return Event_P;

   -------------------------------------------
   --  Events posted to run after a delay.  --
   -------------------------------------------

   --  How many?
   function Number_Of_After_Events (On : Event_Queue_P) return Natural;
   --  Return the At_Index'th event (in order of posting).
   function After_Event (On : Event_Queue_P;
                         At_Index : Positive) return Event_P;
   --  Return the delay for the At_Index'th event (in order of
   --  posting).
   function How_Long_After (On : Event_Queue_P;
                            At_Index : Positive) return Duration;

   ---------------------------------------------
   --  Events posted to run at a later time.  --
   ---------------------------------------------

   --  How many?
   function Number_Of_Later_Events (On : Event_Queue_P) return Natural;
   --  Return the At_Index'th event (in order of posting).
   function Later_Event (On : Event_Queue_P;
                         At_Index : Positive) return Event_P;
   --  Return the time-to-fire for the At_Index'th event (in
   --  order of posting).
   function When_Later (On : Event_Queue_P;
                        At_Index : Positive) return Time.Time;

   -----------------------------
   --  Events set on a Timer. --
   -----------------------------

   --  Returns the event held on the Timer (or null if none). Use one
   --  of the above query interfaces to check the time concerned.
   function Event_Of (The_Timer : Timer) return Event_P;

end ColdFrame.Events_G.Standard_G.Inspection_G;
