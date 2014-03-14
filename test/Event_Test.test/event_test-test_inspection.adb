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

with AUnit.Assertions; use AUnit.Assertions;

with Ada.Calendar;
with Ada.Real_Time;
with Ada.Unchecked_Deallocation;
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
   is new ColdFrame.Project.Events.Instance_Base with record
      T : ColdFrame.Project.Events.Timer;
   end record;

   function State_Image (This : Instance) return String;

   function State_Image (This : Instance) return String is
      pragma Unreferenced (This);
   begin
      return "*none*";
   end State_Image;

   type Handle is access Instance;
   procedure Free is new Ada.Unchecked_Deallocation (Instance, Handle);

   The_Instance : Handle;

   -------------------
   --  Test events  --
   -------------------

   type Self_Event (For_The_Instance : not null access Instance)
      is new ColdFrame.Project.Events.Instance_Event_Base (For_The_Instance)
   with record
      Payload : Integer;
   end record;

   procedure Handler (For_The_Event : Self_Event);

   type Event (For_The_Instance : not null access Instance)
      is new ColdFrame.Project.Events.Instance_Event_Base (For_The_Instance)
   with record
      Payload : Integer;
   end record;

   procedure Handler (For_The_Event : Event);

   type Timer_Event (For_The_Instance : not null access Instance)
      is new ColdFrame.Project.Events.Instance_Event_Base (For_The_Instance)
   with record
      Payload : Integer;
   end record;

   procedure Handler (For_The_Event : Timer_Event);


   procedure Handler (For_The_Event : Self_Event) is
      pragma Unreferenced (For_The_Event);
   begin
      null;
   end Handler;

   procedure Handler (For_The_Event : Event) is
      I : Instance renames For_The_Event.For_The_Instance.all;
      S : constant ColdFrame.Project.Events.Event_P
        := new Self_Event (I'Unchecked_Access);
   begin
      Self_Event (S.all).Payload := For_The_Event.Payload + 1000;
      ColdFrame.Project.Events.Post_To_Self (S, Q);
   end Handler;

   procedure Handler (For_The_Event : Timer_Event) is
      I : Instance renames For_The_Event.For_The_Instance.all;
      T : constant ColdFrame.Project.Events.Event_P
        := new Timer_Event (I'Unchecked_Access);
   begin
      Timer_Event (T.all).Payload := For_The_Event.Payload + 1000;
      ColdFrame.Project.Events.Set (I.T,
                                    To_Fire => T,
                                    On => Q,
                                    After => 2.0);
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
      pragma Unreferenced (C);
      N : Natural;
      pragma Unreferenced (N);
      E : ColdFrame.Project.Events.Event_P;
      pragma Unreferenced (E);
      D : Duration;
      pragma Unreferenced (D);
      T : ColdFrame.Project.Times.Time;
      pragma Unreferenced (T);
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
         N := Inspection.Number_Of_Immediate_Class_Events (Q);
         Assert (False,
                 "Number_Of_Immediate_Class_Events should have failed");
      exception
         when Inspection.Started => null;
      end;
      begin
         E := Inspection.Immediate_Class_Event (Q, 1);
         Assert (False, "Immediate_Class_Event should have failed");
      exception
         when Inspection.Started => null;
      end;
      begin
         N := Inspection.Number_Of_Immediate_Instance_Events (Q);
         Assert (False,
                 "Number_Of_Immediate_Instance_Events should have failed");
      exception
         when Inspection.Started => null;
      end;
      begin
         E := Inspection.Immediate_Instance_Event (Q, 1);
         Assert (False, "Immediate_Instance_Event should have failed");
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
   procedure Check_Self_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Check_Self_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
      E : aliased Event (The_Instance);
   begin
      Assert (Inspection.Number_Of_Self_Events (Q) = 0,
              "number of self events not 0");
      E.Payload := 1;
      Handler (E);
      E.Payload := 2;
      Handler (E);
      Assert (Inspection.Number_Of_Self_Events (Q) = 2,
              "number of self events not 2");
      Assert (Event (Inspection.Self_Event (Q, 1).all).Payload = 1001,
              "wrong payload in first event");
      Assert (Event (Inspection.Self_Event (Q, 2).all).Payload = 1002,
              "wrong payload in second event");
      declare
         Ev : ColdFrame.Project.Events.Event_P;
         pragma Unreferenced (Ev);
      begin
         Ev := Inspection.Self_Event (Q, 3);
         Assert (False, "should have raised exception");
      exception
         when Inspection.Not_Found => null;
      end;
   end Check_Self_Events;


   --  Can retrieve standard posted events.
   procedure Check_Posting_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Check_Posting_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
      E : ColdFrame.Project.Events.Event_P;
   begin
      Assert (Inspection.Number_Of_Immediate_Class_Events (Q) = 0,
              "number of immediate class events not 0");
      Assert (Inspection.Number_Of_Immediate_Instance_Events (Q) = 0,
              "number of immediate instance events not 0");
      E := new Event (The_Instance);
      Event (E.all).Payload := 1;
      ColdFrame.Project.Events.Post (E, Q);
      E := new Event (The_Instance);
      Event (E.all).Payload := 2;
      ColdFrame.Project.Events.Post (E, Q);
      Assert (Inspection.Number_Of_Immediate_Instance_Events (Q) = 2,
              "number of immediate instance events not 2");
      Assert (Event (Inspection.Immediate_Instance_Event (Q, 1).all).Payload
                = 1,
              "wrong payload in first event");
      Assert (Event (Inspection.Immediate_Instance_Event (Q, 2).all).Payload
                = 2,
              "wrong payload in second event");
      begin
         E := Inspection.Immediate_Instance_Event (Q, 3);
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
      pragma Unreferenced (C);
      E : ColdFrame.Project.Events.Event_P;
   begin
      Assert (Inspection.Number_Of_After_Events (Q) = 0,
              "number of after events not 0");
      E := new Event (The_Instance);
      Event (E.all).Payload := 1;
      ColdFrame.Project.Events.Post (E, Q, To_Fire_After => 1.0);
      E := new Event (The_Instance);
      Event (E.all).Payload := 2;
      ColdFrame.Project.Events.Post (E, Q, To_Fire_After => 0.5);
      Assert (Inspection.Number_Of_After_Events (Q) = 2,
              "number of after events not 2");
      E := Inspection.After_Event (Q, 1);
      Assert (Event (Inspection.After_Event (Q, 1).all).Payload = 1,
              "wrong payload in first event");
      Assert (Inspection.How_Long_After (Q, 1) = 1.0,
              "wrong delay in first event");
      Assert (Event (Inspection.After_Event (Q, 2).all).Payload = 2,
              "wrong payload in second event");
      Assert (Inspection.How_Long_After (Q, 2) = 0.5,
              "wrong delay in second event");
      begin
         E := Inspection.After_Event (Q, 3);
         Assert (False, "After_Event should have raised exception");
      exception
         when Inspection.Not_Found => null;
      end;
      declare
         D : Duration;
         pragma Unreferenced (D);
      begin
         D := Inspection.How_Long_After (Q, 3);
         Assert (False, "How_Long_After should have raised exception");
      exception
         when Inspection.Not_Found => null;
      end;
   end Check_After_Events;


   --  Can retrieve events to run 'at' a time.
   procedure Check_Later_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Check_Later_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
      E : ColdFrame.Project.Events.Event_P;
      use type Ada.Calendar.Time;
      use type Ada.Real_Time.Time;
      T1 : constant ColdFrame.Project.Times.Time
        := ColdFrame.Project.Times.Create (Ada.Calendar.Clock + 1.0);
      T2 : constant ColdFrame.Project.Times.Time
        := ColdFrame.Project.Times.Create
        (Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (500));
      use type ColdFrame.Project.Times.Time;
   begin
      Assert (Inspection.Number_Of_Later_Events (Q) = 0,
              "number of later events not 0");
      E := new Event (The_Instance);
      Event (E.all).Payload := 1;
      ColdFrame.Project.Events.Post (E, Q, To_Fire_At => T1);
      E := new Event (The_Instance);
      Event (E.all).Payload := 2;
      ColdFrame.Project.Events.Post (E, Q, To_Fire_At => T2);
      Assert (Inspection.Number_Of_Later_Events (Q) = 2,
              "number of later events not 2");
      E := Inspection.Later_Event (Q, 1);
      Assert (Event (Inspection.Later_Event (Q, 1).all).Payload = 1,
              "wrong payload in first event");
      Assert (Inspection.When_Later (Q, 1) = T1,
              "wrong delay in first event");
      Assert (Event (Inspection.Later_Event (Q, 2).all).Payload = 2,
              "wrong payload in second event");
      Assert (Inspection.When_Later (Q, 2) = T2,
              "wrong delay in second event");
      begin
         E := Inspection.Later_Event (Q, 3);
         Assert (False, "Later_Event should have raised exception");
      exception
         when Inspection.Not_Found => null;
      end;
      declare
         T : ColdFrame.Project.Times.Time;
         pragma Unreferenced (T);
      begin
         T := Inspection.When_Later (Q, 3);
         Assert (False, "When_Later should have raised exception");
      exception
         when Inspection.Not_Found => null;
      end;
   end Check_Later_Events;


   --  Timer-related operations
   procedure Check_Timer_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Check_Timer_Events
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
      E : ColdFrame.Project.Events.Event_P;
      use type ColdFrame.Project.Events.Event_P;
   begin
      E := new Timer_Event (The_Instance);
      Timer_Event (E.all).Payload := 1;
      ColdFrame.Project.Events.Set (The_Instance.T,
                                    On => Q,
                                    To_Fire => E,
                                    After => 1.0);
      Assert (Inspection.Number_Of_After_Events (Q) = 1,
              "number of after events not 1");
      Assert (Inspection.After_Event (Q, 1) = E,
              "wrong event on queue");
      Assert (Inspection.How_Long_After (Q, 1) = 1.0,
              "wrong delay in first event");
      Assert (Inspection.Event_Of (The_Instance.T) = E,
              "wrong event on timer");
      Inspection.Fire (The_Instance.T);
      Assert (Inspection.Event_Of (The_Instance.T) /= E,
              "same event on timer");
      Assert (Timer_Event
                (Inspection.Event_Of (The_Instance.T).all).Payload = 1001,
              "wrong payload on new event");
      --  NB! the fired event hasn't been removed from the event
      --  queue.
      Assert (Inspection.Number_Of_After_Events (Q) = 2,
              "new number of after events not 2");
      Assert (Inspection.How_Long_After (Q, 2) = 2.0,
              "wrong delay in new event");
   end Check_Timer_Events;


   ----------------------------
   --  Framework extensions  --
   ----------------------------

   procedure Register_Tests (C : in out Test_Case) is
   begin
      Registration.Register_Routine
        (C, Inspect_Started_Queue'Access, "Inspect started queue");
      --  Can't run next test because it posts to self outside an
      --  event handler. Could have an event handler that does this
      --  and then handle synchronously? (the queue has to be stopped!)
--        Register_Routine
--          (C, Check_Self_Events'Access, "Self events");
      Registration.Register_Routine
        (C, Check_Posting_Events'Access, "Standard events");
      Registration.Register_Routine
        (C, Check_After_Events'Access, "Events to run after a delay");
      Registration.Register_Routine
        (C, Check_Later_Events'Access, "Events to run at a time");
      Registration.Register_Routine
        (C, Check_Timer_Events'Access, "Inspecting, firing Timers");
   end Register_Tests;

   function Name (C : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (C);
   begin
      return new String'("Inspection");
   end Name;

   procedure Set_Up (C : in out Test_Case) is
      pragma Unreferenced (C);
   begin
      Q := new ColdFrame.Project.Events.Standard.Event_Queue_Base
        (Start_Started => False,
         Priority => System.Default_Priority,
         Storage_Size => 20_000);
      The_Instance := new Instance;
   end Set_Up;

   procedure Tear_Down (C :  in out Test_Case) is
      pragma Unreferenced (C);
   begin
      --  Normally this would be generated code.
      ColdFrame.Project.Events.Stop (Q);
      ColdFrame.Project.Events.Tear_Down (Q);
      Free (The_Instance);
   end Tear_Down;

end Event_Test.Test_Inspection;
