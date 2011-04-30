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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;
with Ada.Strings.Unbounded;

with Ada.Calendar;
with ColdFrame.Exceptions;
with ColdFrame.Project.Events.Standard.Inspection;
with ColdFrame.Project.Events.Standard.Test;
with ColdFrame.Project.Times;
with System;
with Unit_Testing.Events;
with Unit_Testing.Initialize;
with Unit_Testing.Arr.Unit_Test;
with Unit_Testing.Normal.Unit_Test;
with Unit_Testing.Public.Unit_Test;
with Unit_Testing.Singleton.Unit_Test;
with Unit_Testing.Tear_Down;

package body Unit_Testing.Suite is


   package CPE renames ColdFrame.Project.Events;
   package CPESI renames ColdFrame.Project.Events.Standard.Inspection;

   use type CPE.Event_P;


   procedure Public_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Public_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
   begin
      Public.Unit_Test.Set_X (42);
      Assert (Public.Get_X = 42,
              "Public.Get_X wrong");
      Assert (Public.Unit_Test.Get_X = 42,
              "Public.Unit_Test.Get_X wrong");
      Public.Unit_Test.Set_Y (43);
      Assert (Public.Get_Y = 43,
              "Public.Get_Y wrong");
      Assert (Public.Unit_Test.Get_Y = 43,
              "Public.Unit_Test.Get_Y wrong");
   end Public_Tests;


   procedure Arr_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Arr_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      H : Arr.Handle;
      S, T : Arr.Unit_Test.Timer_P;
      use type Arr.Unit_Test.State;
   begin
      H := Arr.Create ((Id => True));
      Arr.Unit_Test.Set_X (42);
      Assert (Arr.Get_X  = 42,
              "Arr.Get_X wrong");
      Assert (Arr.Unit_Test.Get_X  = 42,
              "Arr.Unit_Test.Get_X wrong");
      Arr.Unit_Test.Set_Y (H, 43);
      Assert (Arr.Get_Y (H) = 43,
              "Arr.Get_Y wrong");
      Assert (Arr.Unit_Test.Get_Y (H) = 43,
              "Arr.Unit_Test.Get_Y wrong");
      S := Arr.Unit_Test.Access_S;
      Assert (CPESI.Event_Of (S.all) = null,
              "there is an event on S");
      Arr.Post_C;
      Assert (CPESI.Event_Of (S.all).all in Arr.C'Class,
              "the event on S is of the wrong class");
      T := Arr.Unit_Test.Access_T (H);
      Assert (CPESI.Event_Of (T.all) = null,
              "there is an event on T");
      Assert (Arr.Unit_Test.Get_State_Machine_State (H)
                = Arr.Unit_Test.Initial,
              "state machine in wrong initial state");
      declare
         Ev : Arr.E (H);
      begin
         Arr.Handler (Ev);
      end;
      Assert (Arr.Unit_Test.Get_State_Machine_State (H)
                = Arr.Unit_Test.Final,
              "state machine not in final state");
      Arr.Unit_Test.Set_State_Machine_State (H,
                                             To => Arr.Unit_Test.Initial);
      declare
         Ev : Arr.E (H);
      begin
         Arr.Handler (Ev);
      end;
      Assert (Arr.Unit_Test.Get_State_Machine_State (H) =
                Arr.Unit_Test.Final,
              "state machine not in final state (2)");
   end Arr_Tests;


   procedure Normal_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Normal_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      H : Normal.Handle;
      S, T : Normal.Unit_Test.Timer_P;
      use type Normal.Unit_Test.State;
   begin
      H := Normal.Create;
      Normal.Unit_Test.Set_X (42);
      Assert (Normal.Get_X  = 42,
              "Normal.Get_X wrong");
      Assert (Normal.Unit_Test.Get_X  = 42,
              "Normal.Unit_Test.Get_X wrong");
      Normal.Unit_Test.Set_Y (H, 43);
      Assert (Normal.Get_Y (H) = 43,
              "Normal.Get_Y wrong");
      Assert (Normal.Unit_Test.Get_Y (H) = 43,
              "Normal.Unit_Test.Get_Y wrong");
      S := Normal.Unit_Test.Access_S;
      Assert (CPESI.Event_Of (S.all) = null,
              "there is an event on S");
      Normal.Post_C;
      Assert (CPESI.Event_Of (S.all).all in Normal.C'Class,
              "the event on S is of the wrong class");
      T := Normal.Unit_Test.Access_T (H);
      Assert (CPESI.Event_Of (T.all) = null,
              "there is an event on T");
      Assert (Normal.Unit_Test.Get_State_Machine_State (H)
                = Normal.Unit_Test.Initial,
              "state machine in wrong initial state");
      declare
         Ev : Normal.E (H);
      begin
         Normal.Handler (Ev);
      end;
      Assert (Normal.Unit_Test.Get_State_Machine_State (H)
                = Normal.Unit_Test.Final,
              "state machine not in final state");
      Normal.Unit_Test.Set_State_Machine_State
        (H,
         To => Normal.Unit_Test.Initial);
      declare
         Ev : Normal.E (H);
      begin
         Normal.Handler (Ev);
      end;
      Assert (Normal.Unit_Test.Get_State_Machine_State (H) =
                Normal.Unit_Test.Final,
              "state machine not in final state (2)");
   end Normal_Tests;


   procedure Singleton_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Singleton_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      H : Singleton.Handle;
      S, T : Singleton.Unit_Test.Timer_P;
      use type Singleton.Unit_Test.State;
   begin
      H := Singleton.Find;
      Singleton.Unit_Test.Set_X (42);
      Assert (Singleton.Get_X  = 42,
              "Singleton.Get_X wrong");
      Assert (Singleton.Unit_Test.Get_X  = 42,
              "Singleton.Unit_Test.Get_X wrong");
      Singleton.Unit_Test.Set_Y (43);
      Assert (Singleton.Get_Y = 43,
              "Singleton.Get_Y wrong");
      Assert (Singleton.Unit_Test.Get_Y = 43,
              "Singleton.Unit_Test.Get_Y wrong");
      S := Singleton.Unit_Test.Access_S;
      Assert (CPESI.Event_Of (S.all) = null,
              "there is an event on S");
      Singleton.Post_C;
      Assert (CPESI.Event_Of (S.all).all in Singleton.C'Class,
              "the event on S is of the wrong class");
      T := Singleton.Unit_Test.Access_T;
      Assert (CPESI.Event_Of (T.all) = null,
              "there is an event on T");
      Assert (Singleton.Unit_Test.Get_State_Machine_State
                = Singleton.Unit_Test.Initial,
              "state machine in wrong initial state");
      declare
         Ev : Singleton.E (H);
      begin
         Singleton.Handler (Ev);
      end;
      Assert (Singleton.Unit_Test.Get_State_Machine_State
                = Singleton.Unit_Test.Final,
              "state machine not in final state");
      Singleton.Unit_Test.Set_State_Machine_State
        (To => Singleton.Unit_Test.Initial);
      declare
         Ev : Singleton.E (H);
      begin
         Singleton.Handler (Ev);
      end;
      Assert (Singleton.Unit_Test.Get_State_Machine_State =
                Singleton.Unit_Test.Final,
              "state machine not in final state (2)");
   end Singleton_Tests;


   procedure Test_Only_Operations_Fail
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Only_Operations_Fail
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
   begin
      declare
         Dummy : Boolean;
         pragma Unreferenced (Dummy);
      begin
         Dummy := CPE.Is_Set
           (The_Timer => Singleton.Unit_Test.Access_S.all,
            On => Events.Dispatcher);
         Assert (False, "excpected exception");
      exception
         when ColdFrame.Exceptions.Use_Error => null;
      end;
      declare
         Dummy : ColdFrame.Project.Times.Time;
         pragma Unreferenced (Dummy);
      begin
         Dummy := CPE.Expires_At
           (The_Timer => Singleton.Unit_Test.Access_S.all,
            On => Events.Dispatcher);
         Assert (False, "excpected exception");
      exception
         when ColdFrame.Exceptions.Use_Error => null;
      end;
      begin
         CPE.Wait_Until_Idle (Events.Dispatcher);
         Assert (False, "expected exception");
      exception
         when ColdFrame.Exceptions.Use_Error => null;
      end;
   end Test_Only_Operations_Fail;


   procedure Test_Operations
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Operations
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
      use type Ada.Calendar.Time;
      Firing_Time : constant ColdFrame.Project.Times.Time
        := ColdFrame.Project.Times.Create
        (From_Time => Ada.Calendar.Clock + 0.5);
      use type ColdFrame.Project.Times.Time;
   begin
      Assert (not CPE.Is_Set
                (The_Timer => Singleton.Unit_Test.Access_S.all,
                 On => Events.Dispatcher),
              "timer was set");
      Assert (CPESI.Event_Of (Singleton.Unit_Test.Access_S.all) = null,
              "event present on timer");
      CPE.Set (The_Timer => Singleton.Unit_Test.Access_S.all,
               On => Events.Dispatcher,
               To_Fire => new Singleton.C,
               At_Time => Firing_Time);
      Assert (CPE.Is_Set
                (The_Timer => Singleton.Unit_Test.Access_S.all,
                 On => Events.Dispatcher),
              "timer not set");
      Assert (CPESI.Event_Of (Singleton.Unit_Test.Access_S.all) /= null,
              "event not present on timer");
      Assert (CPE.Expires_At (The_Timer => Singleton.Unit_Test.Access_S.all,
                              On => Events.Dispatcher) = Firing_Time,
              "wrong firing time");
   end Test_Operations;


   type Standard_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (C : in out Standard_Case);

   function Name (C : Standard_Case)
                 return Ada.Strings.Unbounded.String_Access;

   procedure Set_Up (C : in out Standard_Case);

   procedure Tear_Down (C :  in out Standard_Case);

   procedure Register_Tests (C : in out Standard_Case) is
   begin
      Register_Routine
        (C,
         Public_Tests'Access,
         "Public class");
      Register_Routine
        (C,
         Arr_Tests'Access,
         "Array class");
      Register_Routine
        (C,
         Normal_Tests'Access,
         "Normal class");
      Register_Routine
        (C,
         Singleton_Tests'Access,
         "Singleton class");
      Register_Routine
        (C,
         Test_Only_Operations_Fail'Access,
         "Test-only operations fail");
   end Register_Tests;

   function Name (C : Standard_Case)
                 return Ada.Strings.Unbounded.String_Access is
      pragma Warnings (Off, C);
   begin
      return new String'("Unit_Testing Standard queue");
   end Name;

   procedure Set_Up (C : in out Standard_Case) is
      pragma Warnings (Off, C);
      Q : constant CPE.Event_Queue_P := new CPE.Standard.Event_Queue_Base
        (Start_Started => False,
         Priority => System.Default_Priority,
         Storage_Size => 20_000);
   begin
      Unit_Testing.Initialize (Q);
   end Set_Up;

   procedure Tear_Down (C :  in out Standard_Case) is
      pragma Warnings (Off, C);
   begin
      Unit_Testing.Tear_Down;
   end Tear_Down;

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (C : in out Test_Case);

   function Name (C : Test_Case)
                 return Ada.Strings.Unbounded.String_Access;

   procedure Set_Up (C : in out Test_Case);

   procedure Tear_Down (C :  in out Test_Case);

   procedure Register_Tests (C : in out Test_Case) is
   begin
      Register_Routine
        (C,
         Public_Tests'Access,
         "Public class");
      Register_Routine
        (C,
         Arr_Tests'Access,
         "Array class");
      Register_Routine
        (C,
         Normal_Tests'Access,
         "Normal class");
      Register_Routine
        (C,
         Singleton_Tests'Access,
         "Singleton class");
      Register_Routine
        (C,
         Test_Operations'Access,
         "Test-only operations");
   end Register_Tests;

   function Name (C : Test_Case)
                 return Ada.Strings.Unbounded.String_Access is
      pragma Warnings (Off, C);
   begin
      return new String'("Unit_Testing Test queue");
   end Name;

   procedure Set_Up (C : in out Test_Case) is
      pragma Warnings (Off, C);
      Q : constant CPE.Event_Queue_P := new CPE.Standard.Test.Event_Queue_Base
        (Start_Started => False,
         Priority => System.Default_Priority,
         Storage_Size => 20_000);
   begin
      Unit_Testing.Initialize (Q);
   end Set_Up;

   procedure Tear_Down (C :  in out Test_Case) is
      pragma Warnings (Off, C);
   begin
      Unit_Testing.Tear_Down;
   end Tear_Down;

   function Suite
     return AUnit.Test_Suites.Access_Test_Suite is
      use AUnit.Test_Suites;
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Standard_Case);
      Add_Test (Result, new Test_Case);
      return Result;
   end Suite;

end Unit_Testing.Suite;
