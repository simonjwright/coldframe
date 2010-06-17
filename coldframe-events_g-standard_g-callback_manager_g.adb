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

--  $RCSfile: coldframe-events_g-standard_g-callback_manager_g.adb,v $
--  $Revision: 215173d4855f $
--  $Date: 2010/06/17 21:54:03 $
--  $Author: simonjwright $

with Ada.Task_Identification;

package body ColdFrame.Events_G.Standard_G.Callback_Manager_G is

   package body Callback_Manager_G is

      Dispatcher : Event_Queue_P;

      package Inner_Callback is new ColdFrame.Callbacks (T);

      procedure Local_Callback_Handler (Value : T);


      procedure Register (In_The_Context_Of : Event_Queue_P)
      is
         pragma Assert (In_The_Context_Of /= null,
                        "can't register null Dispatcher");
         pragma Assert (Dispatcher = null,
                        "already registered");
      begin
         Dispatcher := In_The_Context_Of;
         Callback.Register (Local_Callback_Handler'Unrestricted_Access);
      end Register;


      procedure Register
        (The_Callback_Procedure : Callback.Callback)
      is
      begin
         Inner_Callback.Register (The_Callback_Procedure.all'Access);
      end Register;


      procedure Deregister
        (The_Callback_Procedure : Callback.Callback)
      is
      begin
         Inner_Callback.Deregister (The_Callback_Procedure.all'Access);
      end Deregister;


      procedure Handler (Ev : Callback_Event)
      is
      begin
         Inner_Callback.Call_Callbacks (Ev.Value);
      end Handler;


      procedure Local_Callback_Handler (Value : T)
      is
         Dispatcher_Task : Standard_G.Dispatcher
           renames Standard_G.Event_Queue_Base (Dispatcher.all).The_Dispatcher;
         use type Ada.Task_Identification.Task_Id;
      begin
         if Ada.Task_Identification.Current_Task
           = Dispatcher_Task'Identity then
            Inner_Callback.Call_Callbacks (Value);
         else
            declare
               Ev : constant Event_P := new Callback_Event;
            begin
               Callback_Event (Ev.all).Value := Value;
               Post (Ev, On => Dispatcher);
            end;
         end if;
      end Local_Callback_Handler;


   end Callback_Manager_G;

end ColdFrame.Events_G.Standard_G.Callback_Manager_G;
