with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;

with ColdFrame.Exceptions;
with ColdFrame.Project.Events.Standard.Inspection;
with ColdFrame.Project.Times;
with System.Assertions;

package body Event_Test.Test_Inspection is


   Q : ColdFrame.Project.Events.Event_Queue_P;

   package Inspection renames ColdFrame.Project.Events.Standard.Inspection;


   --  Can't inspect a started queue.
   procedure Inspect_Started_Queue
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Inspect_Started_Queue
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      N : Natural;
      pragma Warnings (Off, N);
      E : ColdFrame.Project.Events.Event_P;
      pragma Warnings (Off, N);
      D : Duration;
      T : ColdFrame.Project.Times.Time;
   begin
      ColdFrame.Project.Events.Start (Q);

      begin
         N := Inspection.Number_Of_Self_Events (Q);
         Assert (False, "Number_Of_Self_Events should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end;
      begin
         E := Inspection.Self_Event (Q, 1);
         Assert (False, "Self_Event should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end;

      begin
         N := Inspection.Number_Of_Now_Events (Q);
         Assert (False, "Number_Of_Now_Events should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end;
      begin
         E := Inspection.Now_Event (Q, 1);
         Assert (False, "Now_Event should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end;

      begin
         N := Inspection.Number_Of_After_Events (Q);
         Assert (False, "Number_Of_After_Events should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end;
      begin
         E := Inspection.After_Event (Q, 1);
         Assert (False, "After_Event should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end;
      begin
         D := Inspection.How_Long_After (Q, 1);
         Assert (False, "How_Long_After should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end;

      begin
         N := Inspection.Number_Of_Later_Events (Q);
         Assert (False, "Number_Of_Later_Events should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end;
      begin
         E := Inspection.Later_Event (Q, 1);
         Assert (False, "Later_Event should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end;
      begin
         T := Inspection.When_Later (Q, 1);
         Assert (False, "When_Later should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end;

   end Inspect_Started_Queue;

--    procedure
--       (C : in out AUnit.Test_Cases.Test_Case'Class);
--     procedure
--       (C : in out AUnit.Test_Cases.Test_Case'Class) is
--        pragma Warnings (Off, C);
--        Ev : constant ColdFrame.Project.Events.Event_P
--          := new Recipient.Information;
--        Inf : Recipient.Information renames Recipient.Information (Ev.all);
--     begin
--        Inf.Payload := (Ordinal => 1000,
--                        Expected_At => ColdFrame.Project.Calendar.Clock);
--        ColdFrame.Project.Events.Post (Ev, On => Q);
--        Assert (Recipient.Get_Ordinal = 1000,
--                "wrong ordinal" & Recipient.Get_Ordinal'Img);
--        Assert (abs Recipient.Get_Offset < 0.02,
--                "wrong time" & Recipient.Get_Offset'Img);
--     end ;

   procedure Register_Tests (C : in out Test_Case) is
   begin
      Register_Routine
        (C, Inspect_Started_Queue'Access, "Inspect started queue");
   end Register_Tests;

   function Name (C : Test_Case) return String_Access is
      pragma Warnings (Off, C);
   begin
      return new String'("Inspection");
   end Name;

   procedure Set_Up (C : in out Test_Case) is
      pragma Warnings (Off, C);
   begin
      Q := new ColdFrame.Project.Events.Standard.Event_Queue_Base
        (Start_Started => False,
         Priority => System.Default_Priority,
         Storage_Size => 20_000);
   end Set_Up;

   procedure Tear_Down (C :  in out Test_Case) is
      pragma Warnings (Off, C);
   begin
      ColdFrame.Project.Events.Stop (Q);
      ColdFrame.Project.Events.Tear_Down (Q);
   end Tear_Down;

end Event_Test.Test_Inspection;
