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

--  $RCSfile: coldframe-events_g-test_g.adb,v $
--  $Revision: 253a6ad430b0 $
--  $Date: 2002/07/25 05:03:12 $
--  $Author: simon $

with Ada.Exceptions;
with ColdFrame.Exceptions;

package body ColdFrame.Events_G.Test_G is


   --  We use an extension of Event_Queue_Base so as to achieve
   --  dispatching depending on the Event_Queue type, even though we
   --  don't need the Event_Queue itself for either Timer
   --  operation. Remember that Timers are not tagged.


   function Is_Set (The_Timer : Timer;
                    On : Event_Queue) return Boolean is
      pragma Warnings (Off, On);
   begin

      return The_Timer.The_Entry /= null;

   end Is_Set;


   function Expires_At (The_Timer : Timer;
                        On : Event_Queue) return Time.Time is
      pragma Warnings (Off, On);
   begin

      if The_Timer.The_Entry = null then

         Ada.Exceptions.Raise_Exception
           (Exceptions.Use_Error'Identity,
            "attempt to find expiry time of a timer that wasn't set");

      end if;

      return The_Timer.The_Entry.Time_To_Fire;

   end Expires_At;


end ColdFrame.Events_G.Test_G;
