with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;

with Event_Test.Initialize;
with Event_Test.Tear_Down;

with Event_Test.Recipient;
with Event_Test.Events;

with ColdFrame.Exceptions;
with ColdFrame.Project.Events;
with ColdFrame.Project.Event_Support;

package body Event_Test.Test_Class is

   --  A class event can be created and queued to its class, to arrive
   --  immediately.
   procedure Immediate_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Immediate_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
   begin
      Inf.Payload := (Ordinal => 1000,
                      Expected_At => Clock);
      ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Recipient.Get_Ordinal = 1000,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Immediate_Event;

   --  A class event can be created and queued to its class, to arrive
   --  after a delay.
   procedure Delayed_Event_After
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delayed_Event_After
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
   begin
      Inf.Payload := (Ordinal => 1001,
                      Expected_At => Clock + 2.2);
      ColdFrame.Project.Events.Post (Ev,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 2.2);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Recipient.Get_Ordinal = 1001,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Delayed_Event_After;

   --  A class event can be created and queued to its class, to arrive
   --  at a time.
   procedure Delayed_Event_At
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delayed_Event_At
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
   begin
      Inf.Payload := (Ordinal => 1002,
                      Expected_At => Clock + 2.2);
      ColdFrame.Project.Events.Post
        (Ev,
         On => Events.Dispatcher,
         To_Fire_At =>
           ColdFrame.Project.Event_Support.Signature.From_Now (2.2));
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Recipient.Get_Ordinal = 1002,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Delayed_Event_At;

   --  Events queued for delayed delivery will not inhibit events
   --  queued for immediate delivery.
   procedure Delayed_Event_Preceded
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delayed_Event_Preceded
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev1 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf1 : Recipient.Information renames Recipient.Information (Ev1.all);
      Ev2 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf2 : Recipient.Information renames Recipient.Information (Ev2.all);
   begin
      Inf1.Payload := (Ordinal => 1003,
                       Expected_At => Clock + 2.2);
      ColdFrame.Project.Events.Post (Ev1,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 2.2);
      Inf2.Payload := (Ordinal => 1004,
                       Expected_At => Clock);
      ColdFrame.Project.Events.Post (Ev2,
                                     On => Events.Dispatcher);
      delay 1.2;
      Assert (Recipient.Get_Ordinal = 1004,
              "wrong ordinal (a)" & Recipient.Get_Ordinal'Img);
      Assert (abs Recipient.Get_Offset < 0.02,
              "wrong time (a)" & Recipient.Get_Offset'Img);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Recipient.Get_Ordinal = 1003,
              "wrong ordinal (b)" & Recipient.Get_Ordinal'Img);
      Assert (abs Recipient.Get_Offset < 0.04, -- seems close to 0.2 on orm
              "wrong time (b)" & Recipient.Get_Offset'Img);
   end Delayed_Event_Preceded;

   --  Events will be delivered in order even if posted out of order.
   procedure Delayed_Events_In_Order
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delayed_Events_In_Order
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev1 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf1 : Recipient.Information renames Recipient.Information (Ev1.all);
      Ev2 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf2 : Recipient.Information renames Recipient.Information (Ev2.all);
   begin
      Inf1.Payload := (Ordinal => 1005,
                       Expected_At => Clock + 2.2);
      ColdFrame.Project.Events.Post (Ev1,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 2.2);
      Inf2.Payload := (Ordinal => 1006,
                       Expected_At => Clock + 1.1);
      ColdFrame.Project.Events.Post (Ev2,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 1.1);
      delay 1.2;
      Assert (Recipient.Get_Ordinal = 1006,
              "wrong ordinal (a)" & Recipient.Get_Ordinal'Img);
      Assert (abs Recipient.Get_Offset < 0.02,
              "wrong time (a)" & Recipient.Get_Offset'Img);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Recipient.Get_Ordinal = 1005,
              "wrong ordinal (b)" & Recipient.Get_Ordinal'Img);
      Assert (abs Recipient.Get_Offset < 0.04, -- seems close to 0.2 on orm
              "wrong time (b)" & Recipient.Get_Offset'Img);
   end Delayed_Events_In_Order;

   --  A Timer type will be provided, to allow delayed events to be
   --  retracted.
   procedure Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
      T : ColdFrame.Project.Events.Timer;
   begin
      Inf.Payload := (Ordinal => 1007,
                      Expected_At => Clock + 2.2);
      ColdFrame.Project.Events.Set (T,
                                    On => Events.Dispatcher,
                                    To_Fire => Ev,
                                    After => 2.2);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Recipient.Get_Ordinal = 1007,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Timer_Event;

   --  An event can be retracted (by unsetting the timer that holds
   --  it) after its firing time has arrived and before it has
   --  actually been dispatched.
   procedure Retract_Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Retract_Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev1 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf1 : Recipient.Information renames Recipient.Information (Ev1.all);
      Ev2 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Wait;
      Inf2 : Recipient.Wait renames Recipient.Wait (Ev2.all);
      T : ColdFrame.Project.Events.Timer;
   begin
      --  Ev1 will fire after 2.2 seconds.
      Inf1.Payload := (Ordinal => 1008,
                       Expected_At => Clock + 2.2);
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
      Assert (Recipient.Get_Ordinal = 0,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
   end Retract_Timer_Event;

   --  Only one event can be held in a timer at once.
   procedure Multiple_Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Multiple_Timer_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev1 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf1 : Recipient.Information renames Recipient.Information (Ev1.all);
      Ev2 : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf2 : Recipient.Information renames Recipient.Information (Ev2.all);
      T : ColdFrame.Project.Events.Timer;
   begin
      Inf1.Payload := (Ordinal => 1009,
                       Expected_At => Clock + 2.2);
      ColdFrame.Project.Events.Set (T,
                                    On => Events.Dispatcher,
                                    To_Fire => Ev1,
                                    After => 2.2);
      begin
         Inf2.Payload := (Ordinal => 1010,
                          Expected_At => Clock + 2.2);
         ColdFrame.Project.Events.Set (T,
                                       On => Events.Dispatcher,
                                       To_Fire => Ev2,
                                       After => 2.2);
         Assert (False, "no exception raised");
      exception
         when ColdFrame.Exceptions.Use_Error =>
            null;
         when others =>
            Assert (False, "wrong exception raised");
      end;
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Recipient.Get_Ordinal = 1009,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
   end Multiple_Timer_Event;

   --  It is illegal to unset a timer that is not holding an event.
   procedure Unset_Unused_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Unset_Unused_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      T : ColdFrame.Project.Events.Timer;
   begin
      ColdFrame.Project.Events.Unset (T,
                                      On => Events.Dispatcher);
      Assert (False, "no exception raised");
   exception
      when ColdFrame.Exceptions.Use_Error =>
         null;
      when others =>
         Assert (False, "wrong exception raised");
   end Unset_Unused_Timer;

   procedure Unset_Used_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Unset_Used_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
      T : ColdFrame.Project.Events.Timer;
   begin
      Inf.Payload := (Ordinal => 1011,
                      Expected_At => Clock + 2.2);
      ColdFrame.Project.Events.Set (T,
                                    On => Events.Dispatcher,
                                    To_Fire => Ev,
                                    After => 2.2);
      delay 2.3;
      Assert (Recipient.Get_Ordinal = 1011,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
      Assert (abs Recipient.Get_Offset < 0.02,
              "wrong time" & Recipient.Get_Offset'Img);
      begin
         ColdFrame.Project.Events.Unset (T,
                                         On => Events.Dispatcher);
         Assert (False, "no exception raised");
      exception
         when ColdFrame.Exceptions.Use_Error =>
            null;
         when others =>
            Assert (False, "wrong exception raised");
      end;
   end Unset_Used_Timer;

   --  If a timer holding an event is deleted, the event is not
   --  delivered.
   procedure Delete_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Information;
      Inf : Recipient.Information renames Recipient.Information (Ev.all);
   begin
      Inf.Payload := (Ordinal => 1012,
                      Expected_At => Clock + 2.2);
      declare
         T : ColdFrame.Project.Events.Timer;
      begin
         ColdFrame.Project.Events.Set (T,
                                       On => Events.Dispatcher,
                                       To_Fire => Ev,
                                       After => 2.2);
         delay 1.1;
      end;
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Recipient.Get_Ordinal = 0,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
   end Delete_Timer;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Immediate_Event'Access, "Simple event");
      Register_Routine
        (T, Delayed_Event_After'Access, "Delayed event (after)");
      Register_Routine
        (T, Delayed_Event_At'Access, "Delayed event (at)");
      Register_Routine
        (T, Delayed_Event_Preceded'Access, "Simple event not held up");
      Register_Routine
        (T, Delayed_Events_In_Order'Access, "Order of arrival");
      Register_Routine
        (T, Retract_Timer_Event'Access, "Retract timer event after firing");
      Register_Routine
        (T, Timer_Event'Access, "Timer event");
      Register_Routine
        (T, Multiple_Timer_Event'Access, "Multiple timer setting");
      Register_Routine
        (T, Unset_Unused_Timer'Access, "Unset unused timer");
      Register_Routine
        (T, Unset_Used_Timer'Access, "Unset used timer");
      Register_Routine
        (T, Delete_Timer'Access, "Delete set timer");
   end Register_Tests;

   function Name (T : Test_Case) return String_Access is
      pragma Warnings (Off, T);
   begin
      return new String'("Class events");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Initialize;
   end Set_Up;

   procedure Tear_Down (T :  in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Tear_Down;
   end Tear_Down;

end Event_Test.Test_Class;
