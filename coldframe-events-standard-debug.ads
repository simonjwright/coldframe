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

--  $RCSfile: coldframe-events-standard-debug.ads,v $
--  $Revision: cd455a8611de $
--  $Date: 2002/03/13 20:06:39 $
--  $Author: simon $

package ColdFrame.Events.Standard.Debug is

   pragma Elaborate_Body;

   ---------------------
   --  Event queuing  --
   ---------------------

   type Event_Queue is new Standard.Event_Queue with private;

   procedure Post (The : Event_P;
                   On : access Event_Queue);

   procedure Set (The : in out Timer;
                  On : access Event_Queue;
                  To_Fire : Event_P;
                  After : Natural_Duration);

   procedure Unset (The : in out Timer;
                    On : access Event_Queue);

private

   type Event_Queue is new Standard.Event_Queue with null record;

   procedure Log_Retraction (The : Event_P;
                             On : access Event_Queue);

   procedure Log_Pre_Dispatch (The : Event_P;
                               On : access Event_Queue);

   procedure Log_Post_Dispatch (The : Event_P;
                                On : access Event_Queue);

end ColdFrame.Events.Standard.Debug;
