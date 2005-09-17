--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  $RCSfile: house_management-test_suite.adb,v $
--  $Revision: 108faef7a173 $
--  $Date: 2005/09/17 09:06:26 $
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
with House_Management.Lamp;
with House_Management.Tear_Down;

package body House_Management.Test_Suite is

   function Get_Boolean
   is new ColdFrame.Stubs.Get_Input_Value (Boolean);
   function Get_Signal_Name
   is new ColdFrame.Stubs.Get_Input_Value (Digital_IO.Signal_Name);

   package Lamps is
      type Case_1 is new Test_Case with private;
   private
      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);
      procedure Set_Up (C : in out Case_1);
      procedure Tear_Down (C : in out Case_1);
   end Lamps;


   package body Lamps is

      --  Check that each Lamp is connected to the correct Signal in
      --  the Digital_IO domain, in the correct sense (ie, turning the
      --  Lamp on sets the Signal to True).
      procedure Turn_On (R : in out AUnit.Test_Cases.Test_Case'Class);
      procedure Turn_On (R : in out AUnit.Test_Cases.Test_Case'Class) is
         pragma Unreferenced (R);
         use type Digital_IO.Signal_Name;
      begin

         --  Initialization creates a number of lamps; each one turns
         --  itself off on creation (to be tested elsewhere). However,
         --  we have to account for them; we can't just not initialize
         --  the domain, because initialization creates all the
         --  domain's singletons, initialises <<class>> attributes,
         --  and calls user {init} operations to, amongst other
         --  things, set up 'specification' instances and
         --  associations.
         Assert (ColdFrame.Stubs.Number_Of_Calls
                   ("Digital_IO.Application.Set_Output") = 4,
                 "wrong number of calls");

         --  Turn on the Basement lamp.
         Lamp.Turn_On (Lamp.Find ((Name => Basement)));
         --  There should have been 5 calls now.
         Assert (ColdFrame.Stubs.Number_Of_Calls
                   ("Digital_IO.Application.Set_Output") = 5,
                 "wrong number of calls (a)");
         --  The 5th call should have been for Lamp D ...
         Assert (Get_Signal_Name ("Digital_IO.Application.Set_Output",
                                  "S",
                                  5) = Digital_IO.Lamp_D,
                 "wrong signal (a)");
         --  ... and it should have been turned on.
         Assert (Get_Boolean ("Digital_IO.Application.Set_Output",
                              "To_State",
                              5),
                 "should have been turned on (a)");

         --  Repeat for the remaining Lamps.
         Lamp.Turn_On (Lamp.Find ((Name => Ground_Floor)));
         Assert (ColdFrame.Stubs.Number_Of_Calls
                   ("Digital_IO.Application.Set_Output") = 6,
                 "wrong number of calls (a)");
         Assert (Get_Signal_Name ("Digital_IO.Application.Set_Output",
                                  "S",
                                  6) = Digital_IO.Lamp_C,
                 "wrong signal (b)");
         Assert (Get_Boolean ("Digital_IO.Application.Set_Output",
                              "To_State",
                              6),
                 "should have been turned on (b)");

         Lamp.Turn_On (Lamp.Find ((Name => First_Floor)));
         Assert (ColdFrame.Stubs.Number_Of_Calls
                   ("Digital_IO.Application.Set_Output") = 7,
                 "wrong number of calls (c)");
         Assert (Get_Signal_Name ("Digital_IO.Application.Set_Output",
                                  "S",
                                  7) = Digital_IO.Lamp_B,
                 "wrong signal (c)");
         Assert (Get_Boolean ("Digital_IO.Application.Set_Output",
                              "To_State",
                              7),
                 "should have been turned on (c)");

         Lamp.Turn_On (Lamp.Find ((Name => Second_Floor)));
         Assert (ColdFrame.Stubs.Number_Of_Calls
                   ("Digital_IO.Application.Set_Output") = 8,
                 "wrong number of calls (d)");
         Assert (Get_Signal_Name ("Digital_IO.Application.Set_Output",
                                  "S",
                                  8) = Digital_IO.Lamp_A,
                 "wrong signal (d)");
         Assert (Get_Boolean ("Digital_IO.Application.Set_Output",
                              "To_State",
                              8),
                 "should have been turned on (d)");

      end Turn_On;

      function Name (C : Case_1) return String_Access is
         pragma Unreferenced (C);
      begin
         return new String'("Lamps.Case_1");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Register_Routine
           (C,
            Turn_On'Access,
            "turn on");
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

   end Lamps;


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Lamps.Case_1);
      return Result;
   end Suite;


end House_Management.Test_Suite;
