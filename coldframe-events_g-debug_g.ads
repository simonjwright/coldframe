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

--  $RCSfile: coldframe-events_g-debug_g.ads,v $
--  $Revision: 2bf0ced495ca $
--  $Date: 2002/09/12 21:00:08 $
--  $Author: simon $

generic

   type Standard_Queue is new Events_G.Event_Queue_Base with private;

package ColdFrame.Events_G.Debug_G is

   pragma Elaborate_Body;

   ---------------------
   --  Event queuing  --
   ---------------------

   type Event_Queue is new Standard_Queue with private;

   procedure Post (The_Event : Event_P;
                   On : access Event_Queue);

   procedure Post_To_Self (The_Event : Event_P;
                           On : access Event_Queue);

   ----------------------
   --  Delayed events  --
   ----------------------

--     procedure Post (The_Event : Event_P;
--                     On : access Event_Queue;
--                     To_Fire_At : Time.Time);

   procedure Post (The_Event : Event_P;
                   On : access Event_Queue;
                   To_Fire_After : Natural_Duration);

   --------------
   --  Timers  --
   --------------

--     procedure Set (The_Timer : in out Timer;
--                    On : access Event_Queue_Base;
--                    To_Fire : Event_P;
--                    At_Time : Time.Time) is abstract;

   procedure Set (The_Timer : in out Timer;
                  On : access Event_Queue;
                  To_Fire : Event_P;
                  After : Natural_Duration);

   procedure Unset (The_Timer : in out Timer;
                    On : access Event_Queue);

private

   type Event_Queue is new Standard_Queue with null record;

   procedure Log_Retraction (The_Event : Event_P;
                             On : access Event_Queue);

   procedure Log_Pre_Dispatch (The_Event : Event_P;
                               On : access Event_Queue);

   procedure Log_Post_Dispatch (The_Event : Event_P;
                                On : access Event_Queue);

end ColdFrame.Events_G.Debug_G;
