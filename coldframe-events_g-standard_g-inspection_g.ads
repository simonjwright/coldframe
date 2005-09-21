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
--  $Revision: f82037aff426 $
--  $Date: 2005/09/21 05:41:29 $
--  $Author: simonjwright $

with ColdFrame.Events_G.Held_Event_Queue_Signature.Inspection_Signature;

generic

   --  Enable queue inspection.
   with package Held_Events_Inspection
      is new Held_Events.Inspection_Signature (<>);

package ColdFrame.Events_G.Standard_G.Inspection_G is

   --  The idea is to be able to see what's on an Event Queue, so you
   --  don't need to test event posting just by seeing the
   --  aftereffects.
   --
   --  It would be reasonable to insist that the queue is in fact
   --  stopped (not started, really).
   --
   --  This could be a child of Events_G, perhaps, though that would
   --  require all sorts of extra (private?) interfaces.
   --
   --  Handling the 'time-to-fire' events is going to be a tad iffy.

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
   --  Return the At_Index'th event (in increasing delay order).
   function After_Event (On : Event_Queue_P;
                         At_Index : Positive) return Event_P;
   --  Return the delay for the At_Index'th event (in increasing delay
   --  order).
   function How_Long_After (On : Event_Queue_P;
                            At_Index : Positive) return Duration;

   ---------------------------------------------
   --  Events posted to run at a later time.  --
   ---------------------------------------------

   --  How many?
   function Number_Of_Later_Events (On : Event_Queue_P) return Natural;
   --  Return the At_Index'th event (in increasing time-to-fire
   --  order).
   function Later_Event (On : Event_Queue_P;
                         At_Index : Positive) return Event_P;
   --  Return the time-to-fire for the At_Index'th event (in
   --  increasing time-to-fire order).
   function When_Later (On : Event_Queue_P;
                        At_Index : Positive) return Time.Time;

end ColdFrame.Events_G.Standard_G.Inspection_G;
