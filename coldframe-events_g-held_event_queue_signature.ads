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

--  $RCSfile: coldframe-events_g-held_event_queue_signature.ads,v $
--  $Revision: c1e2d532a451 $
--  $Date: 2003/03/09 16:09:24 $
--  $Author: simon $

--  Specifies the properties required of a Queue to contain Events
--  that have to be dispatched at some time in the future.
--
--  Time is provided by the Time signature package used by
--  Events_G. The implementations of that Time and this package will
--  be highly coupled.
--
--  The Queue is not expected to provide any form of protection
--  against concurrent access.

with Ada.Real_Time;

generic

   type Queue is limited private;

   --  Managing Events

   with function Is_Empty (Q : Queue) return Boolean;

   with function Next_Event_Time (Q : Queue) return Ada.Real_Time.Time;

   with procedure Pop (Q : in out Queue; The_Head_Event : out Event_P);

   with procedure Add_At_Event (E : Event_P;
                                To_Run_At : Time.Time;
                                On : in out Queue);

   with procedure Add_After_Event (E : Event_P;
                                   To_Run_After : Duration;
                                   On : in out Queue);

   with procedure Start_Processing_After_Events (On : in out Queue);
   --  After events will not be visible until this procedure has been
   --  called.

   with procedure Invalidate_Events (On : Queue;
                                     For_The_Instance : Instance_Base_P);

   with procedure Tear_Down (Q : in out Queue);

package ColdFrame.Events_G.Held_Event_Queue_Signature is
end ColdFrame.Events_G.Held_Event_Queue_Signature;
