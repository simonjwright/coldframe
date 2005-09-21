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

--  $RCSfile: coldframe-events_g-held_event_queue_signature-inspection_signature.ads,v $
--  $Revision: f82037aff426 $
--  $Date: 2005/09/21 05:41:29 $
--  $Author: simonjwright $

--  Specifies the properties required to inspect a Queue.

with Ada.Real_Time;

generic

   with function Number_Of_At_Events (On : Queue) return Natural;
   with function At_Event (On : Queue;
                           At_Index : Positive) return Event_P;
   with function When_At (On : Queue;
                          At_Index : Positive) return Time.Time;

   with function Number_Of_After_Events (On : Queue) return Natural;
   with function After_Event (On : Queue;
                         At_Index : Positive) return Event_P;
   with function How_Long_After (On : Queue;
                                 At_Index : Positive) return Duration;

package ColdFrame.Events_G.Held_Event_Queue_Signature.Inspection_Signature is
private
   --  Turn off GNAT's warnings (5.02a1)
   pragma Warnings (Off, Number_Of_At_Events);
   pragma Warnings (Off, At_Event);
   pragma Warnings (Off, When_At);
   pragma Warnings (Off, Number_Of_After_Events);
   pragma Warnings (Off, After_Event);
   pragma Warnings (Off, How_Long_After);
end ColdFrame.Events_G.Held_Event_Queue_Signature.Inspection_Signature;
