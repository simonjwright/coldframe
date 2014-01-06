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

--  A receiver domain shouldn't assume, when it gets a callback from
--  another (sender) domain, that they are running in the same event
--  queue context (though they often will be).
--
--  This package supports the style of callback/event processing in
--  http://coldframe.sourceforge.net/coldframe/event-use.html#eventqueues.
--
--  See ColdFrame.Project.Events.Callback_Manager for a standard
--  instantiation.
--
--  Each receiver domain instantiates the nested Callback_Manager_G
--  with the sender domain's called-back type and corresponding
--  callback package.
--
--  During initialization of the receiver domain, it registers its
--  callback handler(s) with the instantiated <type>_Callback_Manager.
--
--  It then registers its Events.Dispatcher with the instantiated
--  <type>_Callback_Manager. This registers an internal callback
--  handler with the sender domain.
--
--  Now, when the sender domain's callback is invoked,
--  <type>_Callback_Manager's internal handler creates a class event,
--  carrying the payload; when dispatched (and therefore in the
--  receiver domain's event queue context), its handler calls the
--  receiver domain's callback handlers.

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with ColdFrame.Callbacks;

generic
package ColdFrame.Events_G.Callback_Manager_G is

   generic

      --  The callback type.
      type T is private;

      --  The other domain's callback package.
      with package Callback is new ColdFrame.Callbacks (T);

   package Callback_Manager_G is

      type Callback_Procedure is access procedure (Value : T);
      --  The callback procedure to be invoked with Value.

      procedure Register (In_The_Context_Of : Event_Queue_P);
      --  Notes the Event Queue for this domain, for posting the inner
      --  clas events.

      procedure Register
        (The_Callback_Procedure : Callback.Callback);
      --  When a callback is received from the other domain, this
      --  procedure is to be called in this domain's event context.

      procedure Deregister
        (The_Callback_Procedure : Callback.Callback);
      --  No longer interested in this callback.

      procedure Clear;
      --  Clear the registered Event Queue and all registered
      --  callbacks.

   private

      type Callback_Event is new Event_Base with record
         Value : T;
      end record;

      procedure Handler (Ev : Callback_Event);

   end Callback_Manager_G;

end ColdFrame.Events_G.Callback_Manager_G;
