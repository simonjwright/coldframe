with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;

with Event_Test.Initialize;
with Event_Test.Tear_Down;

with Event_Test.Machine;
with Event_Test.Events;

with ColdFrame.Exceptions;
with ColdFrame.Project.Events;
with ColdFrame.Project.Event_Support;

package body Event_Test.Test_Instance is

   H : Machine.Handle;

   --  An event will be delivered to its instance.
   procedure Simple_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Simple_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Machine.Mark (H);
      Inf : Machine.Mark renames Machine.Mark (Ev.all);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 2000,
                      Expected_At => ColdFrame.Project.Calendar.Clock);

      ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);

      Assert (Machine.Get_Ordinal (H) = 2000,
              "wrong ordinal" & Machine.Get_Ordinal (H)'Img);
   end Simple_Event;

   --  An event to self will be processed before other events.
   procedure Event_To_Self
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Event_To_Self
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Machine.Self (H);
      Inf : Machine.Self renames Machine.Self (Ev.all);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 2001,
                      Expected_At => ColdFrame.Project.Calendar.Clock);
      ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);

      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);

      Assert (Machine.Get_Ordinal (H) = 2002,
              "wrong ordinal" & Machine.Get_Ordinal (H)'Img);
   end Event_To_Self;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Simple_Event'Access, "Simple event");
      Register_Routine
        (T, Event_To_Self'Access, "Event to self");
   end Register_Tests;

   function Name (T : Test_Case) return String_Access is
      pragma Warnings (Off, T);
   begin
      return new String'("Instance events");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Initialize;
      H := Machine.Create;
   end Set_Up;

   procedure Tear_Down (T :  in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Tear_Down;
      H := null;
   end Tear_Down;

end Event_Test.Test_Instance;
