with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;

with Event_Test.Events.Tear_Down;

with ColdFrame.Exceptions;
with ColdFrame.Instances;
with ColdFrame.Project.Events;
with ColdFrame.Project.Event_Support;

package body Event_Test.Test_Engine is

   --  A class event can be created and queued to its class, to arrive
   --  immediately.
--     procedure Immediate_Event
--       (R : in out AUnit.Test_Cases.Test_Case'Class);
--     procedure Immediate_Event
--       (R : in out AUnit.Test_Cases.Test_Case'Class) is
--        pragma Warnings (Off, R);
--        Ev : constant ColdFrame.Project.Events.Event_P
--          := new Recipient.Information;
--        Inf : Recipient.Information renames Recipient.Information (Ev.all);
--     begin
--        Inf.Payload := (Ordinal => 1000,
--                        Expected_At => ColdFrame.Project.Calendar.Clock);
--        ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);
--        ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
--        Assert (Recipient.Get_Ordinal = 1000,
--                "wrong ordinal" & Recipient.Get_Ordinal'Img);
--        Assert (abs Recipient.Get_Offset < 0.02,
--                "wrong time" & Recipient.Get_Offset'Img);
--     end Immediate_Event;

   procedure Register_Tests (T : in out Test_Case) is
   begin
--        Register_Routine
--          (T, Immediate_Event'Access, "Simple event");
      null;
   end Register_Tests;

   function Name (T : Test_Case) return String_Access is
      pragma Warnings (Off, T);
   begin
      return new String'("Event engine");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Event_Test.Events.Initialize;
   end Set_Up;

   procedure Tear_Down (T :  in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Event_Test.Events.Tear_Down;
   end Tear_Down;

end Event_Test.Test_Engine;
