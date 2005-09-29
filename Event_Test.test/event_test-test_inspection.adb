with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;

with Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;

with ColdFrame.Project.Events.Standard.Inspection;
with ColdFrame.Project.Times;
with System;

package body Event_Test.Test_Inspection is

   --------------------
   --  Test globals  --
   --------------------

   Q : ColdFrame.Project.Events.Event_Queue_P;

   ---------------------
   --  Test instance  --
   ---------------------

   type Instance
      is new ColdFrame.Project.Events.Instance_Base with null record;

   function State_Image (This : Instance) return String;

   function State_Image (This : Instance) return String is
      pragma Warnings (Off, This);
   begin
      return "*none*";
   end State_Image;

   The_Instance : aliased Instance;

   -------------------
   --  Test events  --
   -------------------

   type Ev (For_The_Instance : access Instance)
      is new ColdFrame.Project.Events.Instance_Event_Base (For_The_Instance)
   with record
      Payload : Integer;
   end record;

   procedure Handler (For_The_Event : Ev);
   procedure Handler (For_The_Event : Ev) is
      pragma Warnings (Off, For_The_Event);
   begin
      null;
   end Handler;

   -------------
   --  Tests  --
   -------------

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
      pragma Warnings (Off, E);
      D : Duration;
      pragma Warnings (Off, D);
      T : ColdFrame.Project.Times.Time;
      pragma Warnings (Off, T);
   begin
      ColdFrame.Project.Events.Start (Q);

      begin
         N := Inspection.Number_Of_Self_Events (Q);
         Assert (False, "Number_Of_Self_Events should have failed");
      exception
         when Inspection.Started => null;
      end;
      begin
         E := Inspection.Self_Event (Q, 1);
         Assert (False, "Self_Event should have failed");
      exception
         when Inspection.Started => null;
      end;

      begin
         N := Inspection.Number_Of_Now_Events (Q);
         Assert (False, "Number_Of_Now_Events should have failed");
      exception
         when Inspection.Started => null;
      end;
      begin
         E := Inspection.Now_Event (Q, 1);
         Assert (False, "Now_Event should have failed");
      exception
         when Inspection.Started => null;
      end;

      begin
         N := Inspection.Number_Of_After_Events (Q);
         Assert (False, "Number_Of_After_Events should have failed");
      exception
         when Inspection.Started => null;
      end;
      begin
         E := Inspection.After_Event (Q, 1);
         Assert (False, "After_Event should have failed");
      exception
         when Inspection.Started => null;
      end;
      begin
         D := Inspection.How_Long_After (Q, 1);
         Assert (False, "How_Long_After should have failed");
      exception
         when Inspection.Started => null;
      end;

      begin
         N := Inspection.Number_Of_Later_Events (Q);
         Assert (False, "Number_Of_Later_Events should have failed");
      exception
         when Inspection.Started => null;
      end;
      begin
         E := Inspection.Later_Event (Q, 1);
         Assert (False, "Later_Event should have failed");
      exception
         when Inspection.Started => null;
      end;
      begin
         T := Inspection.When_Later (Q, 1);
         Assert (False, "When_Later should have failed");
      exception
         when Inspection.Started => null;
      end;

   end Inspect_Started_Queue;


   --  Can retrieve events posted to self.

   --  Can retrieve standard posted events.
   procedure Check_Posting_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Check_Posting_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      E : ColdFrame.Project.Events.Event_P;
   begin
      Assert (Inspection.Number_Of_Now_Events (Q) = 0,
              "number of now events not 0");
      E := new Ev (The_Instance'Access);
      Ev (E.all).Payload := 1;
      ColdFrame.Project.Events.Post (E, Q);
      E := new Ev (The_Instance'Access);
      Ev (E.all).Payload := 2;
      ColdFrame.Project.Events.Post (E, Q);
      Assert (Inspection.Number_Of_Now_Events (Q) = 2,
              "number of now events not 2");
      Assert (Ev (Inspection.Now_Event (Q, 1).all).Payload = 1,
              "wrong payload in first event");
      Assert (Ev (Inspection.Now_Event (Q, 2).all).Payload = 2,
              "wrong payload in second event");
      begin
         E := Inspection.Now_Event (Q, 3);
         Assert (False, "should have raised exception");
      exception
         when Inspection.Not_Found => null;
      end;
   end Check_Posting_Events;


   --  Can retrieve events to run 'after' a period.
   procedure Check_After_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Check_After_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      E : ColdFrame.Project.Events.Event_P;
   begin
      Assert (Inspection.Number_Of_After_Events (Q) = 0,
              "number of after events not 0");
      E := new Ev (The_Instance'Access);
      Ev (E.all).Payload := 1;
      ColdFrame.Project.Events.Post (E, Q, To_Fire_After => 1.0);
      E := new Ev (The_Instance'Access);
      Ev (E.all).Payload := 2;
      ColdFrame.Project.Events.Post (E, Q, To_Fire_After => 0.5);
      Assert (Inspection.Number_Of_After_Events (Q) = 2,
              "number of after events not 2");
      E := Inspection.After_Event (Q, 1);
      Put_Line (Ada.Tags.Expanded_Name (E.all'Tag));
      Assert (Ev (Inspection.After_Event (Q, 1).all).Payload = 2,
              "wrong payload in first event");
      Assert (Inspection.How_Long_After (Q, 1) = 0.5,
              "wrong delay in first event");
      Assert (Ev (Inspection.After_Event (Q, 2).all).Payload = 1,
              "wrong payload in second event");
      Assert (Inspection.How_Long_After (Q, 2) = 1.0,
              "wrong delay in second event");
      begin
         E := Inspection.After_Event (Q, 3);
         Assert (False, "After_Event should have raised exception");
      exception
         when Inspection.Not_Found => null;
      end;
      declare
         D : Duration;
         pragma Warnings (Off, D);
      begin
         D := Inspection.How_Long_After (Q, 3);
         Assert (False, "How_Long_After should have raised exception");
      exception
         when Inspection.Not_Found => null;
      end;
   end Check_After_Events;


   --  Can retrieve events to run 'at' a time.


   ----------------------------
   --  Framework extensions  --
   ----------------------------

   procedure Register_Tests (C : in out Test_Case) is
   begin
      Register_Routine
        (C, Inspect_Started_Queue'Access, "Inspect started queue");
      Register_Routine
        (C, Check_Posting_Events'Access, "Standard events");
      Register_Routine
        (C, Check_After_Events'Access, "Events to run after a delay");
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
      --  Normally this would be generated code.
      ColdFrame.Project.Events.Stop (Q);
      ColdFrame.Project.Events.Tear_Down (Q);
   end Tear_Down;

end Event_Test.Test_Inspection;
