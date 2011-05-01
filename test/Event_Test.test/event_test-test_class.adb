--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Event_Test.Initialize;
with Event_Test.Tear_Down;

with Event_Test.Recipient;
with Event_Test.Events;

with ColdFrame.Exceptions;
with ColdFrame.Project.Events;
with ColdFrame.Project.Times;

package body Event_Test.Test_Class is

   --  A class event can be created and queued to its class, to arrive
   --  immediately.
   procedure Immediate_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Immediate_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 1000,
                      Expected_At => ColdFrame.Project.Calendar.Clock);
      ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (R,
              Recipient.Get_Ordinal = 1000,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Immediate_Event;

   --  A class event can be created and queued to its class, to arrive
   --  after a delay 0f 0.0 (which is specially handled in the event
   --  queue).
   procedure Delayed_Event_Now
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delayed_Event_Now
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
      use type ColdFrame.Project.Calendar.Time;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 1001,
                      Expected_At => ColdFrame.Project.Calendar.Clock + 0.0);
      ColdFrame.Project.Events.Post (Ev,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 0.0);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (R,
              Recipient.Get_Ordinal = 1001,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Delayed_Event_Now;

   --  A class event can be created and queued to its class, to arrive
   --  after a delay.
   procedure Delayed_Event_After
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delayed_Event_After
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
      use type ColdFrame.Project.Calendar.Time;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 1001,
                      Expected_At => ColdFrame.Project.Calendar.Clock + 2.2);
      ColdFrame.Project.Events.Post (Ev,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 2.2);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (R,
              Recipient.Get_Ordinal = 1001,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Delayed_Event_After;

   --  A class event can be created and queued to its class, to arrive
   --  at a time.
   procedure Delayed_Event_At
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delayed_Event_At
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
      use type ColdFrame.Project.Calendar.Time;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 1002,
                      Expected_At => ColdFrame.Project.Calendar.Clock + 2.2);
      ColdFrame.Project.Events.Post
        (Ev,
         On => Events.Dispatcher,
         To_Fire_At =>
           ColdFrame.Project.Times.From_Now (2.2));
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (R,
              Recipient.Get_Ordinal = 1002,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Delayed_Event_At;

   --  Events queued for delayed delivery will not inhibit events
   --  queued for immediate delivery.
   procedure Delayed_Event_Preceded
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delayed_Event_Preceded
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev1 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf1 : Recipient.Information renames Recipient.Information (Ev1.all);
      Ev2 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf2 : Recipient.Information renames Recipient.Information (Ev2.all);
      use type ColdFrame.Project.Calendar.Time;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf1.Payload := (Ordinal => 1003,
                       Expected_At => ColdFrame.Project.Calendar.Clock + 2.2);
      ColdFrame.Project.Events.Post (Ev1,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 2.2);
      Inf2.Payload := (Ordinal => 1004,
                       Expected_At => ColdFrame.Project.Calendar.Clock);
      ColdFrame.Project.Events.Post (Ev2,
                                     On => Events.Dispatcher);
      delay 1.2;
      Assert (R,
              Recipient.Get_Ordinal = 1004,
              "wrong ordinal (a)" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.02,
              "wrong time (a)" & Recipient.Get_Offset'Img);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (R,
              Recipient.Get_Ordinal = 1003,
              "wrong ordinal (b)" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.04, -- seems close to 0.2 on orm
              "wrong time (b)" & Recipient.Get_Offset'Img);
   end Delayed_Event_Preceded;

   --  Events will be delivered in order even if posted out of order.
   procedure Delayed_Events_In_Order
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delayed_Events_In_Order
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev1 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf1 : Recipient.Information renames Recipient.Information (Ev1.all);
      Ev2 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf2 : Recipient.Information renames Recipient.Information (Ev2.all);
      use type ColdFrame.Project.Calendar.Time;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf1.Payload := (Ordinal => 1005,
                       Expected_At => ColdFrame.Project.Calendar.Clock + 2.2);
      ColdFrame.Project.Events.Post (Ev1,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 2.2);
      Inf2.Payload := (Ordinal => 1006,
                       Expected_At => ColdFrame.Project.Calendar.Clock + 1.1);
      ColdFrame.Project.Events.Post (Ev2,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 1.1);
      delay 1.2;
      Assert (R,
              Recipient.Get_Ordinal = 1006,
              "wrong ordinal (a)" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.02,
              "wrong time (a)" & Recipient.Get_Offset'Img);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (R,
              Recipient.Get_Ordinal = 1005,
              "wrong ordinal (b)" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.04, -- seems close to 0.2 on orm
              "wrong time (b)" & Recipient.Get_Offset'Img);
   end Delayed_Events_In_Order;

   --  A Timer type will be provided, to allow delayed events to be
   --  retracted.
   procedure Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
      T : ColdFrame.Project.Events.Timer;
      use type ColdFrame.Project.Calendar.Time;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 1007,
                      Expected_At => ColdFrame.Project.Calendar.Clock + 2.2);
      ColdFrame.Project.Events.Set (T,
                                    On => Events.Dispatcher,
                                    To_Fire => Ev,
                                    After => 2.2);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (R,
              Recipient.Get_Ordinal = 1007,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Timer_Event;

   --  An event can be retracted (by unsetting the timer that holds
   --  it) after its firing time has arrived and before it has
   --  actually been dispatched.
   procedure Retract_Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Retract_Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev1 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf1 : Recipient.Information renames Recipient.Information (Ev1.all);
      Ev2 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Wait;
      Inf2 : Recipient.Wait renames Recipient.Wait (Ev2.all);
      T : ColdFrame.Project.Events.Timer;
      use type ColdFrame.Project.Calendar.Time;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      --  Ev1 will fire after 2.2 seconds.
      Inf1.Payload := (Ordinal => 1008,
                       Expected_At => ColdFrame.Project.Calendar.Clock + 2.2);
      ColdFrame.Project.Events.Set (T,
                                    On => Events.Dispatcher,
                                    To_Fire => Ev1,
                                    After => 2.2);
      --  Ev2 will fire immediately. Its handler waits for 5 seonds, as
      --  instructed.
      Inf2.Payload := (Interval => 5.0);
      ColdFrame.Project.Events.Post (Ev2,
                                     On => Events.Dispatcher);
      --  2.2 seconds in, Ev1 fires. It gets posted, but can't be
      --  dispatched because the dispatcher is still handling Ev1.
      delay 3.5;
      --  After another 1.3 seconds, unset the timer, which retracts
      --  Ev1 even though it's on the dispatcher queue ready to go.
      ColdFrame.Project.Events.Unset (T,
                                      On => Events.Dispatcher);
      --  After another 1.5 seconds, Ev2's handler exits, so Ev1
      --  should be dispatched if it's still there.
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      --  Check it isn't.
      Assert (R,
              Recipient.Get_Ordinal = 0,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
   end Retract_Timer_Event;

   --  Only one event can be held in a timer at once.
   procedure Multiple_Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Multiple_Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev1 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf1 : Recipient.Information renames Recipient.Information (Ev1.all);
      Ev2 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf2 : Recipient.Information renames Recipient.Information (Ev2.all);
      T : ColdFrame.Project.Events.Timer;
      use type ColdFrame.Project.Calendar.Time;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf1.Payload := (Ordinal => 1009,
                       Expected_At => ColdFrame.Project.Calendar.Clock + 2.2);
      ColdFrame.Project.Events.Set (T,
                                    On => Events.Dispatcher,
                                    To_Fire => Ev1,
                                    After => 2.2);
      begin
         Inf2.Payload :=
           (Ordinal => 1010,
            Expected_At => ColdFrame.Project.Calendar.Clock + 2.2);
         ColdFrame.Project.Events.Set (T,
                                       On => Events.Dispatcher,
                                       To_Fire => Ev2,
                                       After => 2.2);
         Assert (R,
                 False, "no exception raised");
      exception
         when ColdFrame.Exceptions.Use_Error =>
            null;
         when others =>
            Assert (R,
                    False, "wrong exception raised");
      end;
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (R,
              Recipient.Get_Ordinal = 1009,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Multiple_Timer_Event;

   --  It is illegal to unset a timer that is not holding an event.
   procedure Unset_Unused_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Unset_Unused_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      T : ColdFrame.Project.Events.Timer;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      ColdFrame.Project.Events.Unset (T,
                                      On => Events.Dispatcher);
      Assert (R,
              False, "no exception raised");
   exception
      when ColdFrame.Exceptions.Use_Error =>
         null;
      when others =>
         Assert (R,
                 False, "wrong exception raised");
   end Unset_Unused_Timer;

   procedure Unset_Used_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Unset_Used_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
      T : ColdFrame.Project.Events.Timer;
      use type ColdFrame.Project.Calendar.Time;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 1011,
                      Expected_At => ColdFrame.Project.Calendar.Clock + 2.2);
      ColdFrame.Project.Events.Set (T,
                                    On => Events.Dispatcher,
                                    To_Fire => Ev,
                                    After => 2.2);
      delay 2.3;
      Assert (R,
              Recipient.Get_Ordinal = 1011,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
      begin
         ColdFrame.Project.Events.Unset (T,
                                         On => Events.Dispatcher);
         Assert (R,
                 False, "no exception raised");
      exception
         when ColdFrame.Exceptions.Use_Error =>
            null;
         when others =>
            Assert (R,
                    False, "wrong exception raised");
      end;
   end Unset_Used_Timer;

   --  If a timer holding an event is deleted, the event is not
   --  delivered.
   procedure Delete_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
      use type ColdFrame.Project.Calendar.Time;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 1012,
                      Expected_At => ColdFrame.Project.Calendar.Clock + 2.2);
      declare
         T : ColdFrame.Project.Events.Timer;
      begin
         ColdFrame.Project.Events.Set (T,
                                       On => Events.Dispatcher,
                                       To_Fire => Ev,
                                       After => 2.2);

         delay 1.1;
         ColdFrame.Project.Events.Finalize (T);
      end;
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (R,
              Recipient.Get_Ordinal = 0,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
   end Delete_Timer;

   --  A class event can be created and queued to its class, to arrive
   --  after a delay (relative to when the queue starts).
   procedure Delayed_Event_After_Start
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delayed_Event_After_Start
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
      use type ColdFrame.Project.Calendar.Time;
   begin
      Inf.Payload := (Ordinal => 1013,
                      Expected_At => ColdFrame.Project.Calendar.Clock + 5.2);
      ColdFrame.Project.Events.Post (Ev,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 2.2);
      delay 3.0;
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (R,
              Recipient.Get_Ordinal = 1013,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (R,
              abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Delayed_Event_After_Start;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Immediate_Event'Access, "Simple event");
      Registration.Register_Routine
        (T, Delayed_Event_Now'Access, "Delayed event (now)");
      Registration.Register_Routine
        (T, Delayed_Event_After'Access, "Delayed event (after)");
      Registration.Register_Routine
        (T, Delayed_Event_At'Access, "Delayed event (at)");
      Registration.Register_Routine
        (T, Delayed_Event_Preceded'Access, "Simple event not held up");
      Registration.Register_Routine
        (T, Delayed_Events_In_Order'Access, "Order of arrival");
      Registration.Register_Routine
        (T, Retract_Timer_Event'Access, "Retract timer event after firing");
      Registration.Register_Routine
        (T, Timer_Event'Access, "Timer event");
      Registration.Register_Routine
        (T, Multiple_Timer_Event'Access, "Multiple timer setting");
      Registration.Register_Routine
        (T, Unset_Unused_Timer'Access, "Unset unused timer");
      Registration.Register_Routine
        (T, Unset_Used_Timer'Access, "Unset used timer");
      Registration.Register_Routine
        (T, Delete_Timer'Access, "Delete set timer");
      Registration.Register_Routine
        (T, Delayed_Event_After_Start'Access, "Delayed event (after start)");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return new String'("Class events");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Initialize;
   end Set_Up;

   procedure Tear_Down (T :  in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Tear_Down;
   end Tear_Down;

end Event_Test.Test_Class;
