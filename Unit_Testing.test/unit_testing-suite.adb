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

--  $RCSfile: unit_testing-suite.adb,v $
--  $Revision: cb6befa37495 $
--  $Date: 2006/03/15 21:54:00 $
--  $Author: simonjwright $

with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;
with Ada.Strings.Unbounded;

with Ada.Exceptions;
with ColdFrame.Project.Events.Standard.Inspection;
with System;
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


   Delivered : Integer;

   type Test_Event is new ColdFrame.Project.Events.Event_Base with record
      Payload : Integer;
   end record;

   procedure Handler (Ev : Test_Event);
   procedure Handler (Ev : Test_Event) is
   begin
      Delivered := Ev.Payload;
   end Handler;


   procedure Public_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Public_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      Original_X, Original_Y : Integer;
   begin
      Original_X := Public.Unit_Test.Get_X;
      Public.Unit_Test.Set_X (Original_X + 1);
      Assert (Public.Unit_Test.Get_X = Original_X + 1,
              "Public.Unit_Test.Get_X wrong");
      Original_Y := Public.Unit_Test.Get_Y;
      Public.Unit_Test.Set_Y (Original_Y + 1);
      Assert (Public.Unit_Test.Get_Y = Original_Y + 1,
              "Public.Unit_Test.Get_Y wrong");
   end Public_Tests;


   procedure Arr_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Arr_Tests
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      H : Arr.Handle;
      Original_X, Original_Y : Integer;
      S, T : Arr.Unit_Test.Timer_P;
   begin
      H := Arr.Create ((Id => True));
      Original_X := Arr.Unit_Test.Get_X;
      Arr.Unit_Test.Set_X (Original_X + 1);
      Assert (Arr.Unit_Test.Get_X  = Original_X + 1,
              "Arr.Unit_Test.Get_X wrong");
      Original_Y := Arr.Unit_Test.Get_Y (H);
      Arr.Unit_Test.Set_Y (H, Original_Y + 1);
      Assert (Arr.Unit_Test.Get_Y (H) = Original_Y + 1,
              "Arr.Unit_Test.Get_Y wrong");
      S := Arr.Unit_Test.Access_S;
      Assert (CPESI.Event_Of (S.all) = null,
              "there is an event on S");
      T := Arr.Unit_Test.Access_T (H);
      Assert (CPESI.Event_Of (T.all) = null,
              "there is an event on T");
   end Arr_Tests;


   type Case_1 is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (C : in out Case_1);

   function Name (C : Case_1) return Ada.Strings.Unbounded.String_Access;

   procedure Set_Up (C : in out Case_1);

   procedure Tear_Down (C :  in out Case_1);

   procedure Register_Tests (C : in out Case_1) is
   begin
      Register_Routine
        (C,
         Public_Tests'Access,
         "Public class");
      Register_Routine
        (C,
         Arr_Tests'Access,
         "Array class");
   end Register_Tests;

   function Name (C : Case_1) return Ada.Strings.Unbounded.String_Access is
      pragma Warnings (Off, C);
   begin
      return new String'("Unit_Testing tests");
   end Name;

   procedure Set_Up (C : in out Case_1) is
      pragma Warnings (Off, C);
      Q : constant CPE.Event_Queue_P := new CPE.Standard.Event_Queue_Base
        (Start_Started => False,
         Priority => System.Default_Priority,
         Storage_Size => 20_000);
   begin
      Unit_Testing.Initialize (Q);
   end Set_Up;

   procedure Tear_Down (C :  in out Case_1) is
      pragma Warnings (Off, C);
   begin
      Unit_Testing.Tear_Down;
   end Tear_Down;

   function Suite
     return AUnit.Test_Suites.Access_Test_Suite is
      use AUnit.Test_Suites;
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Case_1);
      return Result;
   end Suite;

end Unit_Testing.Suite;
