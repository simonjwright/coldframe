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
--  $Revision: 5eaa20121d6b $
--  $Date: 2002/01/27 11:20:12 $
--  $Author: simon $

with Ada.Finalization;
with ColdFrame.Instances;

package ColdFrame.States is

   type Instance_Base is new ColdFrame.Instances.Instance_Base with private;
   --  All Instances with state machines are derived from this type.

   type Event_Base (For_The_Instance : access Instance_Base'Class)
   is abstract tagged limited private;
   --  All Events are derived from this type. For_The_Instance is the
   --  instance to which the event is directed.

   type Event_P is access all Event_Base'Class;

   procedure Handler (This : Event_Base) is abstract;

   Cant_Happen : exception;
   --  Raised when an unexpected Event occurs.

   procedure Log (Event : String; State : String);
   procedure Log (Entering : String);

private

   --  A Terminator is a component of an Instance_Base which is used
   --  to cause removal of any outstanding events for that instance
   --  from the scheduler when the instance is deleted.
   type Terminator (For_The_Instance : access Instance_Base)
   is new Ada.Finalization.Limited_Controlled with null record;

   procedure Finalize (The_Terminator : in out Terminator);

   type Instance_Base is new ColdFrame.Instances.Instance_Base with record
      The_Terminator : Terminator (Instance_Base'Access);
   end record;

   type Event_Base (For_The_Instance : access Instance_Base'Class)
   is abstract tagged limited null record;

end ColdFrame.States;
