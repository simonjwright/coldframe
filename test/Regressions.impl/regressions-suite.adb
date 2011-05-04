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

--  Regression tests for ColdFrame.

with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with ColdFrame.Exceptions;
with ColdFrame.Project.Events.Standard.Test;
with ColdFrame.Project.Serialization;
with ColdFrame.Project.Times;
with System.Assertions;

with Regressions.CB_Callback;
with Regressions.Callback_Type_Callback;
with Regressions.Event_Holder;
with Regressions.Events;
with Regressions.Exception_In_Event_Handler;
with Regressions.Find_Active;
with Regressions.Find_Active_Singleton;
with Regressions.Initialize;
with Regressions.Max_One;
with Regressions.Phoenix;
with Regressions.PT_User;
with Regressions.Preemptable_Test.Collections;
with Regressions.Preemptable_Test.Iterate;
with Regressions.Rule;
with Regressions.Self_Immolator;
with Regressions.Serializable;
with Regressions.Tear_Down;
with Regressions.Test_Preemption;

--  Call up units only have to compile

pragma Warnings (Off);
with Compilation_Regressions.Subunits;
pragma Warnings (On);

--  May not be referenced for released versions
pragma Warnings (Off, Ada.Text_IO);

package body Regressions.Suite is


   --  Check that protected type operations can be <<access>
   OOPT : Access_Operation_Of_Protected_Type;
   pragma Unreferenced (OOPT);


   package Find_Active_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Find_Active_Tests;

   package body Find_Active_Tests is

      procedure Can_Find (C : in out Test_Case'Class);
      procedure Can_Find (C : in out Test_Case'Class) is
         Created : constant Find_Active.Handle
           := Find_Active.Create ((Id => True));
         Found : Find_Active.Handle;
         use type Find_Active.Handle;
      begin
         select
            delay 0.5;
            Assert (C,
                    False,
                    "hang during Find");
         then abort
            Found := Find_Active.Find ((Id => True));
            Assert (C,
                    Found = Created,
                    "didn't find the created instance");
            return;
         end select;
      end Can_Find;

      procedure Cant_Find (C : in out Test_Case'Class);
      procedure Cant_Find (C : in out Test_Case'Class) is
         Created : constant Find_Active.Handle
           := Find_Active.Create ((Id => True));
         pragma Unreferenced (Created);
         Found : Find_Active.Handle;
         use type Find_Active.Handle;
      begin
         select
            delay 0.5;
            Assert (C,
                    False,
                    "hang during Find");
         then abort
            Found := Find_Active.Find ((Id => False));
            Assert (C,
                    Found = null,
                    "found the uncreated instance");
            return;
         end select;
      end Cant_Find;

      procedure Find_Singleton (C : in out Test_Case'Class);
      procedure Find_Singleton (C : in out Test_Case'Class) is
         Found : Find_Active_Singleton.Handle;
         use type Find_Active_Singleton.Handle;
      begin
         select
            delay 0.5;
            Assert (C,
                    False,
                    "hang during Find");
         then abort
            Found := Find_Active_Singleton.Find;
            Assert (C,
                    Found /= null,
                    "didn't find the instance");
            return;
         end select;
      end Find_Singleton;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Find_Active_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Can_Find'Access,
            "can find an instance without hanging");
         Registration.Register_Routine
           (C,
            Cant_Find'Access,
            "can fail to find a non-existent instance without hanging");
         Registration.Register_Routine
           (C,
            Find_Singleton'Access,
            "can find a singleton instance without hanging");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Initialize;
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Find_Active_Tests;


   package Serialization_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Serialization_Tests;

   package body Serialization_Tests is

      --  May have to report the bad value first (length limit on
      --  message, I think).

      procedure Enum (C : in out Test_Case'Class);
      procedure Enum (C : in out Test_Case'Class) is
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
         Assert (C,
                 Image = Expected,
                 "expecting '" & Expected & "', got '" & Image & "'");
      end Enum;

      procedure Enum_Array (C : in out Test_Case'Class);
      procedure Enum_Array (C : in out Test_Case'Class) is
         Expected : constant String
           := "<record name=""Regressions.Enum_Array"">" & ASCII.LF
           & "<field name=""Enum_Array.FALSE"">A</field>" & ASCII.LF
           & "<field name=""Enum_Array.TRUE"">B</field>" & ASCII.LF
           & "</record>";
         Value : constant Serializable.Enum_Array
           := (ColdFrame.Project.Serialization.Base with
                 Payload => (False => A, True => B));
         Image : constant String
           := Serializable.Image (Value);
      begin
         Assert (C,
                 Image = Expected,
                 "got '" & Image & "', expecting '" & Expected & "'");
      end Enum_Array;

      procedure S_Record (C : in out Test_Case'Class);
      procedure S_Record (C : in out Test_Case'Class) is
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
         Assert (C,
                 Image = Expected,
                 "expecting '" & Expected & "', got '" & Image & "'");
      end S_Record;

      procedure S_Record_Array (C : in out Test_Case'Class);
      procedure S_Record_Array (C : in out Test_Case'Class) is
         Expected : constant String
           := "<record name=""Regressions.S_Record_Array"">" & ASCII.LF
           & "<field name=""S_Record_Array.0.I""> 42</field>" & ASCII.LF
           & "<field name=""S_Record_Array.0.D.D""> 0.250000000</field>"
           & ASCII.LF
           & "<field name=""S_Record_Array.0.E"">A</field>" & ASCII.LF
           & "<field name=""S_Record_Array.1.I""> 43</field>" & ASCII.LF
           & "<field name=""S_Record_Array.1.D.D""> 0.500000000</field>"
           & ASCII.LF
           & "<field name=""S_Record_Array.1.E"">B</field>" & ASCII.LF
           & "<field name=""S_Record_Array.2.I""> 44</field>" & ASCII.LF
           & "<field name=""S_Record_Array.2.D.D""> 0.750000000</field>"
           & ASCII.LF
           & "<field name=""S_Record_Array.2.E"">C</field>" & ASCII.LF
           & "</record>";
         Value : constant Serializable.S_Record_Array
           := (ColdFrame.Project.Serialization.Base with
                 Payload =>
                 (0 => (I => 42,
                        D => (D => 0.25),
                        E => Regressions.A),
                  1 => (I => 43,
                        D => (D => 0.5),
                       E => Regressions.B),
                  2 => (I => 44,
                        D => (D => 0.75),
                        E => Regressions.C)));
         Image : constant String
           := Serializable.Image (Value);
      begin
         Assert (C,
                 Image = Expected,
                 "got '" & Image & "', expecting '" & Expected & "'");
      end S_Record_Array;

      procedure Array_Composite (C : in out Test_Case'Class);
      procedure Array_Composite (C : in out Test_Case'Class) is
         Expected : constant String
           := "<record name=""Regressions.Array_Composite"">" & ASCII.LF
           & "<field name=""E.FALSE"">A</field>" & ASCII.LF
           & "<field name=""E.TRUE"">B</field>" & ASCII.LF
           & "<field name=""S.0.I""> 42</field>" & ASCII.LF
           & "<field name=""S.0.D.D""> 0.250000000</field>"
           & ASCII.LF
           & "<field name=""S.0.E"">A</field>" & ASCII.LF
           & "<field name=""S.1.I""> 43</field>" & ASCII.LF
           & "<field name=""S.1.D.D""> 0.500000000</field>"
           & ASCII.LF
           & "<field name=""S.1.E"">B</field>" & ASCII.LF
           & "<field name=""S.2.I""> 44</field>" & ASCII.LF
           & "<field name=""S.2.D.D""> 0.750000000</field>"
           & ASCII.LF
           & "<field name=""S.2.E"">C</field>" & ASCII.LF
           & "</record>";
         Value : constant Serializable.Array_Composite
           := (ColdFrame.Project.Serialization.Base with
                 Payload =>
                 (E => (False => A,
                        True => B),
                  S => (0 => (I => 42,
                              D => (D => 0.25),
                              E => Regressions.A),
                        1 => (I => 43,
                              D => (D => 0.5),
                              E => Regressions.B),
                        2 => (I => 44,
                              D => (D => 0.75),
                              E => Regressions.C))));
         Image : constant String
           := Serializable.Image (Value);
      begin
         Assert (C,
                 Image = Expected,
                 "got '" & Image & "', expecting '" & Expected & "'");
      end Array_Composite;

      procedure Null_Record (C : in out Test_Case'Class);
      procedure Null_Record (C : in out Test_Case'Class) is
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
         Assert (C,
                 Image = Expected,
                 "expecting '" & Expected & "', got '" & Image & "'");
      end Null_Record;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Serialization_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Enum'Access,
            "can image an enum");
         Registration.Register_Routine
           (C,
            Enum_Array'Access,
            "can image an array of enums");
         Registration.Register_Routine
           (C,
            S_Record'Access,
            "can image a record");
         Registration.Register_Routine
           (C,
            Array_Composite'Access,
            "can image a record of arrays");
         Registration.Register_Routine
           (C,
            S_Record_Array'Access,
            "can image an array of records");
         Registration.Register_Routine
           (C,
            Null_Record'Access,
            "can image a null record");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Initialize;
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Serialization_Tests;


   package Callback_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
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
         pragma Unreferenced (V);
      begin
         Regressions.CB_Callback.Deregister (C4'Access);
      end C4;

      procedure Call_Callbacks_1 (C : in out Test_Case'Class);
      procedure Call_Callbacks_1 (C : in out Test_Case'Class) is
      begin
         Put_Line ("** No exceptions expected **");
         Regressions.CB_Callback.Call_Callbacks (CB'(Reason => 1));
         Assert (C,
                 C1_Called and C2_Called and C3_Called,
                 "not all got called");
      end Call_Callbacks_1;

      procedure Call_Callbacks_2 (C : in out Test_Case'Class);
      procedure Call_Callbacks_2 (C : in out Test_Case'Class) is
      begin
         Put_Line ("** Exception_1 expected **");
         Regressions.CB_Callback.Call_Callbacks (CB'(Reason => 2));
         Assert (C,
                 C1_Called and C2_Called and C3_Called,
                 "not all got called");
      end Call_Callbacks_2;

      procedure Call_Callbacks_3 (C : in out Test_Case'Class);
      procedure Call_Callbacks_3 (C : in out Test_Case'Class) is
      begin
         Put_Line ("** Exception_1, Exception_2 expected **");
         Regressions.CB_Callback.Call_Callbacks (CB'(Reason => 3));
         Assert (C,
                 C1_Called and C2_Called and C3_Called,
                 "not all got called");
      end Call_Callbacks_3;

      procedure Call_Callbacks_4 (C : in out Test_Case'Class);
      procedure Call_Callbacks_4 (C : in out Test_Case'Class) is
      begin
         Put_Line ("** Exception_1, Exception_2 and Exception_3 expected **");
         Regressions.CB_Callback.Register (C4'Access);
         Regressions.CB_Callback.Call_Callbacks (CB'(Reason => 4));
         Assert (C,
                 C1_Called and C2_Called and C3_Called,
                 "not all got called");
      end Call_Callbacks_4;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Callback_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Call_Callbacks_1'Access,
            "call 3 callbacks (no exceptions)");
         Registration.Register_Routine
           (C,
            Call_Callbacks_2'Access,
            "call 3 callbacks (exception in first)");
         Registration.Register_Routine
           (C,
            Call_Callbacks_3'Access,
            "call 3 callbacks (excrption in first two)");
         Registration.Register_Routine
           (C,
            Call_Callbacks_4'Access,
            "call 4 callbacks (excrption in first three, deregister fourth)");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
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
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Callback_Tests;


   package Null_Event_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Null_Event_Tests;

   package body Null_Event_Tests is

      T : ColdFrame.Project.Events.Timer;

      procedure Standard_Posting (C : in out Test_Case'Class);
      procedure Standard_Posting (C : in out Test_Case'Class) is
      begin
         ColdFrame.Project.Events.Post (null,
                                        On => Events.Dispatcher);
         Assert (C, False, "standard posting should have failed");
      exception
         when Constraint_Error => null;
      end Standard_Posting;

      procedure Post_To_Self (C : in out Test_Case'Class);
      procedure Post_To_Self (C : in out Test_Case'Class) is
      begin
         ColdFrame.Project.Events.Post_To_Self (null,
                                                On => Events.Dispatcher);
         Assert (C, False, "self posting should have failed");
      exception
         when Constraint_Error => null;
      end Post_To_Self;

      procedure Post_At (C : in out Test_Case'Class);
      procedure Post_At (C : in out Test_Case'Class) is
      begin
         ColdFrame.Project.Events.Post
           (null,
            On => Events.Dispatcher,
            To_Fire_At =>
              ColdFrame.Project.Times.Create
              (From_Time => Ada.Calendar.Clock));
         Assert (C, False, "posting ""at"" should have failed");
      exception
         when Constraint_Error => null;
      end Post_At;

      procedure Post_After (C : in out Test_Case'Class);
      procedure Post_After (C : in out Test_Case'Class) is
      begin
         ColdFrame.Project.Events.Post (null,
                                        On => Events.Dispatcher,
                                        To_Fire_After => 0.1);
         Assert (C, False, "posting ""after""should have failed");
      exception
         when Constraint_Error => null;
      end Post_After;

      procedure Set_At (C : in out Test_Case'Class);
      procedure Set_At (C : in out Test_Case'Class) is
      begin
         ColdFrame.Project.Events.Set
           (T,
            On => Events.Dispatcher,
            To_Fire => null,
            At_Time =>
              ColdFrame.Project.Times.Create
              (From_Time => Ada.Calendar.Clock));
         Assert (C, False, "setting ""at"" should have failed");
      exception
         when Constraint_Error => null;
      end Set_At;

      procedure Set_After (C : in out Test_Case'Class);
      procedure Set_After (C : in out Test_Case'Class) is
      begin
         ColdFrame.Project.Events.Set (T,
                                       On => Events.Dispatcher,
                                       To_Fire => null,
                                       After => 0.1);
         Assert (C, False, "setting ""after"" should have failed");
      exception
         when Constraint_Error => null;
      end Set_After;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Null_Event_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Standard_Posting'Access,
            "null event (standard post)");
         Registration.Register_Routine
           (C,
            Post_To_Self'Access,
            "null event (post to self)");
         Registration.Register_Routine
           (C,
            Post_At'Access,
            "null event (post at)");
         Registration.Register_Routine
           (C,
            Post_After'Access,
            "null event (post after)");
         Registration.Register_Routine
           (C,
            Set_At'Access,
            "null event (set at)");
         Registration.Register_Routine
           (C,
            Set_After'Access,
            "null event (set after)");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Initialize
           (new ColdFrame.Project.Events.Standard.Event_Queue);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Null_Event_Tests;


   package Max_One_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Max_One_Tests;

   package body Max_One_Tests is

      procedure Delete_With_Identifier (C : in out Test_Case'Class);
      procedure Delete_With_Identifier (C : in out Test_Case'Class) is
         MOH : Max_One.Handle;
         pragma Unreferenced (MOH);
         use type Max_One.Handle;
      begin
         MOH := Max_One.Create ((Id => 42));
         Max_One.Delete ((Id => 42));
         Assert (C, Max_One.Find = null, "instance still present");
      end Delete_With_Identifier;

      procedure Delete_With_Handle (C : in out Test_Case'Class);
      procedure Delete_With_Handle (C : in out Test_Case'Class) is
         MOH : Max_One.Handle;
         use type Max_One.Handle;
      begin
         MOH := Max_One.Create ((Id => 42));
         Max_One.Delete (MOH);
         Assert (C, MOH = null, "handle not null");
         Assert (C, Max_One.Find = null, "instance still present");
      end Delete_With_Handle;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Max_One_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Delete_With_Identifier'Access,
            "delete by identifier");
         Registration.Register_Routine
           (C,
            Delete_With_Handle'Access,
            "delete by handle");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Initialize;
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Max_One_Tests;


   package Callback_Registration_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Callback_Registration_Tests;

   package body Callback_Registration_Tests is

      procedure Callback (C : Callback_Type);
      procedure Callback (C : Callback_Type) is
         pragma Unreferenced (C);
      begin
         null;
      end Callback;

      procedure Multiple_Registrations (C : in out Test_Case'Class);
      procedure Multiple_Registrations (C : in out Test_Case'Class) is
      begin
         Callback_Type_Callback.Register (Callback'Unrestricted_Access);
         Callback_Type_Callback.Register (Callback'Unrestricted_Access);
         Assert (C, False, "re-registration should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end Multiple_Registrations;

      procedure Deregistration (C : in out Test_Case'Class);
      procedure Deregistration (C : in out Test_Case'Class) is
      begin
         Callback_Type_Callback.Deregister (Callback'Unrestricted_Access);
         Assert (C,
                 False,
                 "deregistration of unregistered should have failed");
      exception
         when System.Assertions.Assert_Failure => null;
      end Deregistration;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Callback_Registration_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Multiple_Registrations'Access,
            "multiple registrations");
         Registration.Register_Routine
           (C,
            Deregistration'Access,
            "deregistration when not registered");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Initialize;
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Callback_Registration_Tests;


   package Tearing_Down_Active_Timers_Tests is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Tearing_Down_Active_Timers_Tests;

   package body Tearing_Down_Active_Timers_Tests is

      procedure Instance_Exists (C : in out Test_Case'Class);
      procedure Instance_Exists (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
         EHH : constant Event_Holder.Handle := Event_Holder.Create;
         pragma Unreferenced (EHH);
      begin
         delay 0.01;
         ColdFrame.Project.Events.Start (Events.Dispatcher);
         ColdFrame.Project.Events.Wait_Until_Idle
           (Events.Dispatcher,
            Ignoring_Timers => True);
      end Instance_Exists;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Tearing_Down_Active_Timers_Tests.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Instance_Exists'Access,
            "instance exists");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Initialize
           (new ColdFrame.Project.Events.Standard.Test.Event_Queue);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Tearing_Down_Active_Timers_Tests;


   package Event_Handler_Exceptions is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Event_Handler_Exceptions;

   package body Event_Handler_Exceptions is

      procedure Standard_Event (C : in out Test_Case'Class);
      procedure Standard_Event (C : in out Test_Case'Class) is
         H : Exception_In_Event_Handler.Handle
           := Exception_In_Event_Handler.Create;
      begin
         Put_Line ("** Exception_In_Event_Handler expected **");
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
            Assert (C, False, "failed to delete instance");
      end Standard_Event;

      procedure Timed_Event (C : in out Test_Case'Class);
      procedure Timed_Event (C : in out Test_Case'Class) is
         H : Exception_In_Event_Handler.Handle
           := Exception_In_Event_Handler.Create;
      begin
         Put_Line ("** Exception_In_Event_Handler expected **");
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
            Assert (C, False, "failed to delete instance");
      end Timed_Event;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Event_Handler_Exceptions.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Standard_Event'Access,
            "exception in handler for standard event");
         Registration.Register_Routine
           (C,
            Timed_Event'Access,
            "exception in handler for delayed event");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Initialize
           (new ColdFrame.Project.Events.Standard.Test.Event_Queue);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Event_Handler_Exceptions;


   package Protected_Types_And_Access is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Protected_Types_And_Access;

   package body Protected_Types_And_Access is

      procedure Check_Values (C : in out Test_Case'Class);
      procedure Check_Values (C : in out Test_Case'Class) is
      begin
         Assert (C,
                 not PT_User.Get_State,
                 "value not false");
         PT_User.Set_State (False);
         Assert (C,
                 not PT_User.Get_State,
                 "value not false");
         PT_User.Set_State (True);
         Assert (C,
                 PT_User.Get_State,
                 "value not true");
      end Check_Values;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Protected_Types_And_Access.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Check_Values'Access,
            "check values in protected object");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Initialize
           (new ColdFrame.Project.Events.Standard.Test.Event_Queue);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Protected_Types_And_Access;


   package Task_Deletes_Itself is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Task_Deletes_Itself;

   package body Task_Deletes_Itself is

      procedure Delete_Yourself (C : in out Test_Case'Class);
      procedure Delete_Yourself (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
         H : Self_Immolator.Handle;
      begin
         H := Self_Immolator.Create;
         Put_Line ("** ""Task tried to delete itself"" expected **");
         Self_Immolator.Terminate_Yourself (H);
      end Delete_Yourself;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Task_Deletes_Itself.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Delete_Yourself'Access,
            "task deletes itself");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Initialize
           (new ColdFrame.Project.Events.Standard.Test.Event_Queue);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Task_Deletes_Itself;


   generic
      type Float_Type is digits <>;
      Test_Name : String;
   package Numeric_Overflow is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Numeric_Overflow;

   package body Numeric_Overflow is

      function Divide (Top, Bottom : Float_Type) return Float_Type;
      function Divide (Top, Bottom : Float_Type) return Float_Type is
      begin
         return Top / Bottom;
      end Divide;

      procedure Positive_Infinity (C : in out Test_Case'Class);
      procedure Positive_Infinity (C : in out Test_Case'Class) is
         Result : Float_Type;
         pragma Unreferenced (Result);
      begin
         Result := Divide (+1.0, 0.0);
         Assert (C, False, "should have raised an exception");
      exception
         when Constraint_Error => null;
      end Positive_Infinity;

      procedure Negative_Infinity (C : in out Test_Case'Class);
      procedure Negative_Infinity (C : in out Test_Case'Class) is
         Result : Float_Type;
         pragma Unreferenced (Result);
      begin
         Result := Divide (-1.0, 0.0);
         Assert (C, False, "should have raised an exception");
      exception
         when Constraint_Error => null;
      end Negative_Infinity;

      procedure Not_A_Number (C : in out Test_Case'Class);
      procedure Not_A_Number (C : in out Test_Case'Class) is
         Result : Float_Type;
         pragma Unreferenced (Result);
      begin
         Result := Divide (0.0, 0.0);
         Assert (C, False, "should have raised an exception");
      exception
         when Constraint_Error => null;
      end Not_A_Number;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'(Test_Name & ".Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Positive_Infinity'Unrestricted_Access,
            "positive infinity");
         Registration.Register_Routine
           (C,
            Negative_Infinity'Unrestricted_Access,
            "negative infinity");
         Registration.Register_Routine
           (C,
            Not_A_Number'Unrestricted_Access,
            "not a number");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Initialize
           (new ColdFrame.Project.Events.Standard.Test.Event_Queue);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Numeric_Overflow;
   package Short_Numeric_Overflow
   is new Numeric_Overflow (Real_Without_Bounds, "Short_Numeric_Overflow");
   package Long_Numeric_Overflow
   is new Numeric_Overflow (Furlongs, "Long_Numeric_Overflow");


   package Reflexive_Associations is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Reflexive_Associations;

   package body Reflexive_Associations is

      type Tests is array (Test_Name) of Boolean;

      function Conflicts (From : Test_Name) return Tests;
      function Conflicts (From : Test_Name) return Tests is
         procedure Note (H : Preemptable_Test.Handle);
         procedure Note  is new Preemptable_Test.Iterate (Note);
         Result : Tests := (others => False);
         procedure Note (H : Preemptable_Test.Handle) is
         begin
            Result (Preemptable_Test.Get_Name (H)) := True;
         end Note;
      begin
         Note (Test_Preemption.Conflicts_With_And_May_Be_Preempted_By
                 (Preemptable_Test.Find ((Name => From))));
         return Result;
      end Conflicts;

      function Image (T : Tests) return String;
      function Image (T : Tests) return String is
         Result : String (1 .. 7) := (others => ' ');
      begin
         for C in T'Range loop
            if T (C) then
               declare
                  I : String renames C'Img;
               begin
                  Result (Test_Name'Pos (C) + 1) := I (1);
               end;
            end if;
         end loop;
         return Result;
      end Image;

      procedure Check_Associations (Cs : in out Test_Case'Class);
      procedure Check_Associations (Cs : in out Test_Case'Class) is
         T : Preemptable_Test.Collections.Collection;
      begin
         T := Test_Preemption.Conflicts_With_And_May_Be_Preempted_By
           (Preemptable_Test.Find ((Name => A)));
         Assert (Cs,
                 Preemptable_Test.Collections.Length (T) = 2,
                 "wrong count from A: "
                   & Preemptable_Test.Collections.Length (T)'Img);
         T := Test_Preemption.Conflicts_With_And_May_Be_Preempted_By
           (Preemptable_Test.Find ((Name => B)));
         Assert (Cs,
                 Preemptable_Test.Collections.Length (T) = 1,
                 "wrong count from B: "
                   & Preemptable_Test.Collections.Length (T)'Img);
         T := Test_Preemption.Conflicts_With_And_May_Be_Preempted_By
           (Preemptable_Test.Find ((Name => C)));
         Assert (Cs,
                 Preemptable_Test.Collections.Length (T) = 2,
                 "wrong count from C: "
                   & Preemptable_Test.Collections.Length (T)'Img);
         T := Test_Preemption.Conflicts_With_And_May_Be_Preempted_By
           (Preemptable_Test.Find ((Name => D)));
         Assert (Cs,
                 Preemptable_Test.Collections.Length (T) = 0,
                 "wrong count from D: "
                   & Preemptable_Test.Collections.Length (T)'Img);
         Assert (Cs,
                 Conflicts (A) = Tests'(E | F => True, others => False),
                 "bad conflicts with A: " & Image (Conflicts (A)));
         Assert (Cs,
                 Conflicts (B) = Tests'(F => True, others => False),
                 "bad conflicts with B: "& Image (Conflicts (B)));
         Assert (Cs,
                 Conflicts (C) = Tests'(F | G => True, others => False),
                 "bad conflicts with C: "& Image (Conflicts (C)));
         Assert (Cs,
                 Conflicts (D) = Tests'(others => False),
                 "bad conflicts with D: "& Image (Conflicts (D)));
      end Check_Associations;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Reflexive_Associations.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Check_Associations'Access,
            "check associations");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Initialize
           (new ColdFrame.Project.Events.Standard.Test.Event_Queue);
         Rule.Create
           (Preempter => A,
            Preemptee => E,
            Preempts => False);
         Rule.Create
           (Preempter => A,
            Preemptee => F,
            Preempts => True);
         Rule.Create
           (Preempter => B,
            Preemptee => F,
            Preempts => True);
         Rule.Create
           (Preempter => Regressions.C,
            Preemptee => F,
            Preempts => False);
         Rule.Create
           (Preempter => Regressions.C,
            Preemptee => G,
            Preempts => True);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Tear_Down;
      end Tear_Down;

   end Reflexive_Associations;


   package Completion_After_Final_Action is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Completion_After_Final_Action;

   package body Completion_After_Final_Action is

      procedure Trigger_Final_Action (C : in out Test_Case'Class);
      procedure Trigger_Final_Action (C : in out Test_Case'Class) is
      begin
         declare
            H : constant Phoenix.Handle := Phoenix.Create;
            Ev : Phoenix.Quit (H);
         begin
            Phoenix.Handler (Ev);
         end;
         declare
            H : constant Phoenix.Handle := Phoenix.Find;
            St : constant String := Phoenix.State_Image (H.all);
         begin
            Assert (C,
                    St = "REGRESSIONS.PHOENIX.INITIAL",
                    "incorrect state " & St);
         end;
      end Trigger_Final_Action;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Unreferenced (C);
      begin
         return new String'("Completion_After_Final_Action.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Trigger_Final_Action'Access,
            "final action creates new instance");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Initialize
           (new ColdFrame.Project.Events.Standard.Test.Event_Queue);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         Regressions.Tear_Down;
      end Tear_Down;

   end Completion_After_Final_Action;


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
      AUnit.Test_Suites.Add_Test (Result,
                                  new Protected_Types_And_Access.Case_1);
      AUnit.Test_Suites.Add_Test (Result,
                                  new Task_Deletes_Itself.Case_1);
      AUnit.Test_Suites.Add_Test (Result,
                                  new Short_Numeric_Overflow.Case_1);
      AUnit.Test_Suites.Add_Test (Result,
                                  new Long_Numeric_Overflow.Case_1);
      AUnit.Test_Suites.Add_Test (Result,
                                  new Reflexive_Associations.Case_1);
      AUnit.Test_Suites.Add_Test (Result,
                                  new Completion_After_Final_Action.Case_1);
      return Result;
   end Suite;


end Regressions.Suite;
