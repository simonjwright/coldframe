--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  $RCSfile: house_management-test_suite.adb,v $
--  $Revision: 001d120f3df6 $
--  $Date: 2005/09/08 05:39:55 $
--  $Author: simonjwright $

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ColdFrame.Project.Events.Standard.Test;
with ColdFrame.Stubs;
with Digital_IO.Application;
with Digital_IO.Initialize;
with Digital_IO.Tear_Down;
with House_Management.Initialize;
with House_Management.Tear_Down;

package body House_Management.Test_Suite is


   package Initialization is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Initialization;


   package body Initialization is

      function Name (C : Case_1) return String_Access is
         pragma Unreferenced (C);
      begin
         return new String'("Initialize.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         null;
--           Register_Routine
--             (C,
--              'Access,
--              "");
      end Register_Tests;

      procedure Set_Up (C : in out Case_1) is
         pragma Unreferenced (C);
         Q : constant ColdFrame.Project.Events.Event_Queue_P
           := new ColdFrame.Project.Events.Standard.Test.Event_Queue;
      begin
         ColdFrame.Stubs.Set_Up;
         Digital_IO.Initialize (Q);
         House_Management.Initialize (Q);
      end Set_Up;

      procedure Tear_Down (C : in out Case_1) is
         pragma Unreferenced (C);
      begin
         House_Management.Tear_Down;
         Digital_IO.Tear_Down;
         ColdFrame.Stubs.Tear_Down;
      end Tear_Down;

   end Initialization;


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Initialization.Case_1);
      return Result;
   end Suite;


end House_Management.Test_Suite;
