--  $Id: regressions-suite.adb,v 4f0f861e8c91 2004/07/07 20:42:42 simon $
--
--  Regression tests for ColdFrame.

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with ColdFrame.Exceptions;
with ColdFrame.Project.Events.Standard.Test;
with ColdFrame.Project.Serialization;
with ColdFrame.Project.Times;
with System.Assertions;

with Regressions.Callback_Type_Callback;
with Regressions.CB_Callback;
with Regressions.Events;
with Regressions.Event_Holder;
with Regressions.Exception_In_Event_Handler;
with Regressions.Find_Active;
with Regressions.Find_Active_Singleton;
with Regressions.Initialize;
with Regressions.Serializable;
with Regressions.Tear_Down;

--  The following units only have to compile

with Regressions.Bounded_String_ID;
with Regressions.CT_Class;
with Regressions.Child_Uses_Action;
with Regressions.Class_Operation_Access_Modes;
with Regressions.Fixed_String_Class;
with Regressions.Identified_Class;
with Regressions.Max_More;
with Regressions.Max_More_C;
with Regressions.Max_One;
with Regressions.Max_One_C;
with Regressions.One_Enum_ID;
with Regressions.One_Int_ID;
with Regressions.Parent_With_Action;
with Regressions.Public_With_Attributes;
with Regressions.Public_Without_Attributes;
with Regressions.Renaming_Operations_Child;
with Regressions.Renaming_Operations_Parent;
with Regressions.Singleton_With_Attributes;
with Regressions.Singleton_With_Referential_Attribute;
with Regressions.Singleton_Without_Attributes;
with Regressions.Utility;

pragma Warnings (Off, Regressions.Bounded_String_ID);
pragma Warnings (Off, Regressions.CT_Class);
pragma Warnings (Off, Regressions.Child_Uses_Action);
pragma Warnings (Off, Regressions.Class_Operation_Access_Modes);
pragma Warnings (Off, Regressions.Fixed_String_Class);
pragma Warnings (Off, Regressions.Identified_Class);
pragma Warnings (Off, Regressions.Max_More);
pragma Warnings (Off, Regressions.Max_More_C);
pragma Warnings (Off, Regressions.Max_One);
pragma Warnings (Off, Regressions.Max_One_C);
pragma Warnings (Off, Regressions.One_Enum_ID);
pragma Warnings (Off, Regressions.One_Int_ID);
pragma Warnings (Off, Regressions.Parent_With_Action);
pragma Warnings (Off, Regressions.Public_With_Attributes);
pragma Warnings (Off, Regressions.Public_Without_Attributes);
pragma Warnings (Off, Regressions.Renaming_Operations_Child);
pragma Warnings (Off, Regressions.Renaming_Operations_Parent);
pragma Warnings (Off, Regressions.Singleton_With_Attributes);
pragma Warnings (Off, Regressions.Singleton_With_Referential_Attribute);
pragma Warnings (Off, Regressions.Singleton_Without_Attributes);
pragma Warnings (Off, Regressions.Utility);

--  May not be referenced for released versions
pragma Warnings (Off, Ada.Text_IO);

package body Regressions.Suite is


   package Find_Active_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Find_Active_Tests;

   package body Find_Active_Tests is

      procedure Can_Find (C : in out Test_Case'Class);
      procedure Can_Find (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         Created : constant Find_Active.Handle
           := Find_Active.Create ((Id => True));
         Found : Find_Active.Handle;
         use type Find_Active.Handle;
      begin
         select
            delay 0.5;
            Assert (False,
                    "hang during Find");
         then abort
            Found := Find_Active.Find ((Id => True));
            Assert (Found = Created,
                    "didn't find the created instance");
            return;
         end select;
      end Can_Find;

      procedure Cant_Find (C : in out Test_Case'Class);
      procedure Cant_Find (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         Created : constant Find_Active.Handle
           := Find_Active.Create ((Id => True));
         pragma Warnings (Off, Created);
         Found : Find_Active.Handle;
         use type Find_Active.Handle;
      begin
         select
            delay 0.5;
            Assert (False,
                    "hang during Find");
         then abort
            Found := Find_Active.Find ((Id => False));
            Assert (Found = null,
                    "found the uncreated instance");
            return;
         end select;
      end Cant_Find;

      procedure Find_Singleton (C : in out Test_Case'Class);
      procedure Find_Singleton (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         Found : Find_Active_Singleton.Handle;
         use type Find_Active_Singleton.Handle;
      begin
         select
            delay 0.5;
            Assert (False,
                    "hang during Find");
         then abort
            Found := Find_Active_Singleton.Find;
            Assert (Found /= null,
                    "didn't find the instance");
            return;
         end select;
      end Find_Singleton;

      function Name (C : Case_1) return String_Access is
         pragma Warnings (Off, C);
      begin
         return new String'("Find_Active_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Register_Routine
           (C,
            Can_Find'Access,
            "can find an instance without hanging");
         Register_Routine
           (C,
            Cant_Find'Access,
            "can fail to find a non-existent instance without hanging");
         Register_Routine
           (C,
            Find_Singleton'Access,
            "can find a singleton instance without hanging");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Initialize;
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Find_Active_Tests;


   package Serialization_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Serialization_Tests;

   package body Serialization_Tests is

      procedure Enum (C : in out Test_Case'Class);
      procedure Enum (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         Expected : constant String
           := "<record name=""Regressions.Enum"">" & ASCII.LF
           & "<field name=""Enum"">B</field>" & ASCII.LF
           & "</record>";
         Value : constant Serializable.Enum
           := (ColdFrame.Project.Serialization.Base with
                 Payload => B);
         Image : constant String
           := Serializable.Image (Value);
      begin
         Assert (Image = Expected,
                 "expecting '" & Expected & "', got '" & Image & "'");
      end Enum;

      procedure S_Record (C : in out Test_Case'Class);
      procedure S_Record (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         Expected : constant String
           := "<record name=""Regressions.S_Record"">" & ASCII.LF
           & "<field name=""I""> 42</field>" & ASCII.LF
           & "<field name=""D.D""> 0.500000000</field>" & ASCII.LF
           & "<field name=""E"">C</field>" & ASCII.LF
           & "</record>";
         Value : constant Serializable.S_Record
           := (ColdFrame.Project.Serialization.Base with
                 Payload =>
                 (I => 42,
                  D => (D => 0.5),
                  E => Regressions.C));
         Image : constant String
           := Serializable.Image (Value);
      begin
         Assert (Image = Expected,
                 "expecting '" & Expected & "', got '" & Image & "'");
      end S_Record;

      procedure Null_Record (C : in out Test_Case'Class);
      procedure Null_Record (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         Expected : constant String
           := "<record name=""Regressions.Null_Record"">" & ASCII.LF
           & "<field name=""Null_Record"">null</field>" & ASCII.LF
           & "</record>";
         Value : constant Serializable.Null_Record
           := (ColdFrame.Project.Serialization.Base with
                 Payload => (null record));
         Image : constant String
           := Serializable.Image (Value);
      begin
         Assert (Image = Expected,
                 "expecting '" & Expected & "', got '" & Image & "'");
      end Null_Record;

      function Name (C : Case_1) return String_Access is
         pragma Warnings (Off, C);
      begin
         return new String'("Serialization_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Register_Routine
           (C,
            Enum'Access,
            "can image an enum");
         Register_Routine
           (C,
            S_Record'Access,
            "can image a record");
         Register_Routine
           (C,
            Null_Record'Access,
            "can image a null record");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Initialize;
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Serialization_Tests;


   package Callback_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Callback_Tests;

   package body Callback_Tests is

      procedure C1 (V : CB);
      procedure C2 (V : CB);
      procedure C3 (V : CB);
      procedure C4 (V : CB);

      C1_Called : Boolean;
      C2_Called : Boolean;
      C3_Called : Boolean;

      Exception_1 : exception;
      Exception_2 : exception;
      Exception_3 : exception;

      procedure C1 (V : CB) is
      begin
         C1_Called := True;
         if V.Reason > 1 then
            raise Exception_1;
         end if;
      end C1;

      procedure C2 (V : CB) is
      begin
         C2_Called := True;
         if V.Reason > 2 then
            raise Exception_2;
         end if;
      end C2;

      procedure C3 (V : CB) is
      begin
         C3_Called := True;
         if V.Reason > 3 then
            raise Exception_3;
         end if;
      end C3;

      procedure C4 (V : CB) is
         pragma Warnings (Off, V);
      begin
         Regressions.CB_Callback.Deregister (C4'Access);
      end C4;

      procedure Call_Callbacks_1 (C : in out Test_Case'Class);
      procedure Call_Callbacks_1 (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         Regressions.CB_Callback.Call_Callbacks (CB'(Reason => 1));
         Assert (C1_Called and C2_Called and C3_Called,
                 "not all got called");
      end Call_Callbacks_1;

      procedure Call_Callbacks_2 (C : in out Test_Case'Class);
      procedure Call_Callbacks_2 (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         Regressions.CB_Callback.Call_Callbacks (CB'(Reason => 2));
         Assert (C1_Called and C2_Called and C3_Called,
                    "not all got called");
      end Call_Callbacks_2;

      procedure Call_Callbacks_3 (C : in out Test_Case'Class);
      procedure Call_Callbacks_3 (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         Regressions.CB_Callback.Call_Callbacks (CB'(Reason => 3));
         Assert (C1_Called and C2_Called and C3_Called,
                 "not all got called");
      end Call_Callbacks_3;

      procedure Call_Callbacks_4 (C : in out Test_Case'Class);
      procedure Call_Callbacks_4 (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         Regressions.CB_Callback.Register (C4'Access);
         Regressions.CB_Callback.Call_Callbacks (CB'(Reason => 4));
         Assert (C1_Called and C2_Called and C3_Called,
                 "not all got called");
      end Call_Callbacks_4;

      function Name (C : Case_1) return String_Access is
         pragma Warnings (Off, C);
      begin
         return new String'("Callback_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Register_Routine
           (C,
            Call_Callbacks_1'Access,
            "call 3 callbacks (no exceptions)");
         Register_Routine
           (C,
            Call_Callbacks_2'Access,
            "call 3 callbacks (exception in first)");
         Register_Routine
           (C,
            Call_Callbacks_3'Access,
            "call 3 callbacks (excrption in first two)");
         Register_Routine
           (C,
            Call_Callbacks_4'Access,
            "call 4 callbacks (excrption in first three, deregister fourth)");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Initialize;
         Regressions.CB_Callback.Register (C1'Access);
         Regressions.CB_Callback.Register (C2'Access);
         Regressions.CB_Callback.Register (C3'Access);
         C1_Called := False;
         C2_Called := False;
         C3_Called := False;
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Callback_Tests;


   package Null_Event_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Null_Event_Tests;

   package body Null_Event_Tests is

      T : ColdFrame.Project.Events.Timer;

      procedure Standard_Posting (C : in out Test_Case'Class);
      procedure Standard_Posting (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         ColdFrame.Project.Events.Post (null,
                                        On => Events.Dispatcher);
         Assert (False, "standard posting should have failed");
      exception
         when Constraint_Error => null;
      end Standard_Posting;

      procedure Post_To_Self (C : in out Test_Case'Class);
      procedure Post_To_Self (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         ColdFrame.Project.Events.Post_To_Self (null,
                                                On => Events.Dispatcher);
         Assert (False, "self posting should have failed");
      exception
         when Constraint_Error => null;
      end Post_To_Self;

      procedure Post_At (C : in out Test_Case'Class);
      procedure Post_At (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         ColdFrame.Project.Events.Post
           (null,
            On => Events.Dispatcher,
            To_Fire_At =>
              ColdFrame.Project.Times.Create
              (From_Time => Ada.Calendar.Clock));
         Assert (False, "posting ""at"" should have failed");
      exception
         when Constraint_Error => null;
      end Post_At;

      procedure Post_After (C : in out Test_Case'Class);
      procedure Post_After (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         ColdFrame.Project.Events.Post (null,
                                        On => Events.Dispatcher,
                                        To_Fire_After => 0.1);
         Assert (False, "posting ""after""should have failed");
      exception
         when Constraint_Error => null;
      end Post_After;

      procedure Set_At (C : in out Test_Case'Class);
      procedure Set_At (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         ColdFrame.Project.Events.Set
           (T,
            On => Events.Dispatcher,
            To_Fire => null,
            At_Time =>
              ColdFrame.Project.Times.Create
              (From_Time => Ada.Calendar.Clock));
         Assert (False, "setting ""at"" should have failed");
      exception
         when Constraint_Error => null;
      end Set_At;

      procedure Set_After (C : in out Test_Case'Class);
      procedure Set_After (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         ColdFrame.Project.Events.Set (T,
                                       On => Events.Dispatcher,
                                       To_Fire => null,
                                       After => 0.1);
         Assert (False, "setting ""after"" should have failed");
      exception
         when Constraint_Error => null;
      end Set_After;

      function Name (C : Case_1) return String_Access is
         pragma Warnings (Off, C);
      begin
         return new String'("Null_Event_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Register_Routine
           (C,
            Standard_Posting'Access,
            "null event (standard post)");
         Register_Routine
           (C,
            Post_To_Self'Access,
            "null event (post to self)");
         Register_Routine
           (C,
            Post_At'Access,
            "null event (post at)");
         Register_Routine
           (C,
            Post_After'Access,
            "null event (post after)");
         Register_Routine
           (C,
            Set_At'Access,
            "null event (set at)");
         Register_Routine
           (C,
            Set_After'Access,
            "null event (set after)");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Initialize
           (new ColdFrame.Project.Events.Standard.Event_Queue);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Null_Event_Tests;


   package Max_One_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Max_One_Tests;

   package body Max_One_Tests is

      procedure Delete_With_Identifier (C : in out Test_Case'Class);
      procedure Delete_With_Identifier (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         MOH : Max_One.Handle;
         pragma Warnings (Off, MOH);
         use type Max_One.Handle;
      begin
         MOH := Max_One.Create ((Id => 42));
         Max_One.Delete ((Id => 42));
         pragma Assert (Max_One.Find = null, "instance still present");
      end Delete_With_Identifier;

      procedure Delete_With_Handle (C : in out Test_Case'Class);
      procedure Delete_With_Handle (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         MOH : Max_One.Handle;
         use type Max_One.Handle;
      begin
         MOH := Max_One.Create ((Id => 42));
         Max_One.Delete (MOH);
         pragma Assert (MOH = null, "handle not null");
         pragma Assert (Max_One.Find = null, "instance still present");
      end Delete_With_Handle;

      function Name (C : Case_1) return String_Access is
         pragma Warnings (Off, C);
      begin
         return new String'("Max_One_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Register_Routine
           (C,
            Delete_With_Identifier'Access,
            "delete by identifier");
         Register_Routine
           (C,
            Delete_With_Handle'Access,
            "delete by handle");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Initialize;
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Max_One_Tests;


   package Callback_Registration_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Callback_Registration_Tests;

   package body Callback_Registration_Tests is

      procedure Callback (C : Callback_Type);
      procedure Callback (C : Callback_Type) is
         pragma Warnings (Off, C);
      begin
         null;
      end Callback;

      procedure Multiple_Registrations (C : in out Test_Case'Class);
      procedure Multiple_Registrations (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         Callback_Type_Callback.Register (Callback'Unrestricted_Access);
         Callback_Type_Callback.Register (Callback'Unrestricted_Access);
         Assert (False, "re-registration should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end Multiple_Registrations;

      procedure Deregistration (C : in out Test_Case'Class);
      procedure Deregistration (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         Callback_Type_Callback.Deregister (Callback'Unrestricted_Access);
         Assert (False, "deregistration of unregistered should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end Deregistration;

      function Name (C : Case_1) return String_Access is
         pragma Warnings (Off, C);
      begin
         return new String'("Callback_Registration_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Register_Routine
           (C,
            Multiple_Registrations'Access,
            "multiple registrations");
         Register_Routine
           (C,
            Deregistration'Access,
            "deregistration when not registered");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Initialize;
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Callback_Registration_Tests;


   package Tearing_Down_Active_Timers_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Tearing_Down_Active_Timers_Tests;

   package body Tearing_Down_Active_Timers_Tests is

      procedure Instance_Exists (C : in out Test_Case'Class);
      procedure Instance_Exists (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         EHH : constant Event_Holder.Handle := Event_Holder.Create;
         pragma Unreferenced (EHH);
      begin
         delay 0.01;
         ColdFrame.Project.Events.Start (Events.Dispatcher);
         ColdFrame.Project.Events.Wait_Until_Idle
           (Events.Dispatcher,
            Ignoring_Timers => True);
      end Instance_Exists;

      function Name (C : Case_1) return String_Access is
         pragma Warnings (Off, C);
      begin
         return new String'("Tearing_Down_Active_Timers_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Register_Routine
           (C,
            Instance_Exists'Access,
            "instance exists");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Initialize
           (new ColdFrame.Project.Events.Standard.Test.Event_Queue);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Tearing_Down_Active_Timers_Tests;


   package Event_Handler_Exceptions is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Event_Handler_Exceptions;

   package body Event_Handler_Exceptions is

      procedure Standard_Event (C : in out Test_Case'Class);
      procedure Standard_Event (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         H : Exception_In_Event_Handler.Handle
           := Exception_In_Event_Handler.Create;
      begin
         ColdFrame.Project.Events.Post
           (On => Events.Dispatcher,
            The_Event => new Exception_In_Event_Handler.Ev (H));
         ColdFrame.Project.Events.Start (Events.Dispatcher);
         ColdFrame.Project.Events.Wait_Until_Idle
           (Events.Dispatcher,
            Ignoring_Timers => True);
         Exception_In_Event_Handler.Delete (H);
      exception
         when ColdFrame.Exceptions.Use_Error =>
            Assert (False, "failed to delete instance");
      end Standard_Event;

      procedure Timed_Event (C : in out Test_Case'Class);
      procedure Timed_Event (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         H : Exception_In_Event_Handler.Handle
           := Exception_In_Event_Handler.Create;
      begin
         ColdFrame.Project.Events.Post
           (On => Events.Dispatcher,
            The_Event => new Exception_In_Event_Handler.Ev (H),
            To_Fire_After => 0.1);
         ColdFrame.Project.Events.Start (Events.Dispatcher);
         ColdFrame.Project.Events.Wait_Until_Idle
           (Events.Dispatcher,
            Ignoring_Timers => False);
         Exception_In_Event_Handler.Delete (H);
      exception
         when ColdFrame.Exceptions.Use_Error =>
            Assert (False, "failed to delete instance");
      end Timed_Event;

      function Name (C : Case_1) return String_Access is
         pragma Warnings (Off, C);
      begin
         return new String'("Event_Handler_Exceptions.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Register_Routine
           (C,
            Standard_Event'Access,
            "exception in handler for standard event");
         Register_Routine
           (C,
            Timed_Event'Access,
            "exception in handler for delayed event");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Initialize
           (new ColdFrame.Project.Events.Standard.Test.Event_Queue);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Warnings (Off, C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Event_Handler_Exceptions;


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Find_Active_Tests.Case_1);
      AUnit.Test_Suites.Add_Test (Result, new Serialization_Tests.Case_1);
      AUnit.Test_Suites.Add_Test (Result, new Callback_Tests.Case_1);
      AUnit.Test_Suites.Add_Test (Result, new Null_Event_Tests.Case_1);
      AUnit.Test_Suites.Add_Test (Result, new Max_One_Tests.Case_1);
      AUnit.Test_Suites.Add_Test (Result,
                                  new Callback_Registration_Tests.Case_1);
      AUnit.Test_Suites.Add_Test (Result,
                                  new Tearing_Down_Active_Timers_Tests.Case_1);
      AUnit.Test_Suites.Add_Test (Result,
                                  new Event_Handler_Exceptions.Case_1);
      return Result;
   end Suite;


end Regressions.Suite;
