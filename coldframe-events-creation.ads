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

--  $RCSfile: coldframe-events-creation.ads,v $
--  $Revision: e447b4015a27 $
--  $Date: 2002/02/23 13:39:27 $
--  $Author: simon $

package ColdFrame.Events.Creation is

   type Event (For_The_Instance : access Instance_Base'Class)
   is new Instance_Event_Base with private;
   --  Used to pass to the Initial state's Entry procedure. For other
   --  states, there's always an Event of some sort; whether it's the
   --  right sort of event for any entry Actions is another matter.

private

   type Event (For_The_Instance : access Instance_Base'Class)
   is new Instance_Event_Base (For_The_Instance) with null record;

   procedure Handler (This : Event);

end ColdFrame.Events.Creation;
