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

--  $RCSfile: stub_test_suite.adb,v $
--  $Revision: 25d83881d00e $
--  $Date: 2005/03/05 18:32:59 $
--  $Author: simon $

with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;
with Ada.Strings.Unbounded;

with Ada.Exceptions;
with ColdFrame.Stubs;
with Stub_Test.Initialize;
with Stub_Test.Public;
with Stub_Test.Tear_Down;

package body Stub_Test_Suite is


   procedure Set_Integer
   is new ColdFrame.Stubs.Set_Output_Value (Integer);
   procedure Set_Discriminated_Type
   is new ColdFrame.Stubs.Set_Output_Value (Stub_Test.Discriminated_Type);
   procedure Set_Record_Type
   is new ColdFrame.Stubs.Set_Output_Value (Stub_Test.Record_Type);

   function Get_Boolean
   is new ColdFrame.Stubs.Get_Input_Value (Boolean);
   function Get_Integer
   is new ColdFrame.Stubs.Get_Input_Value (Integer);


   procedure Call_With_Return
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Call_With_Return
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
   begin
      Set_Integer ("Stub_Test.Public.Get_Value", "return", 42);
      Set_Integer ("Stub_Test.Public.Get_Value", "return", 24, 3);
      Assert (Stub_Test.Public.Get_Value = 42,
              "wrong value (a)");
      Assert (Stub_Test.Public.Get_Value = 42,
              "wrong value (b)");
      Assert (Stub_Test.Public.Get_Value = 24,
              "wrong value (c)");
      Assert (Stub_Test.Public.Get_Value = 24,
              "wrong value (d)");
   end Call_With_Return;


   procedure Call_With_Exception
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Call_With_Exception
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      I : Integer;
      pragma Warnings (Off, I);
   begin
      Set_Integer ("Stub_Test.Public.Get_Value", "return", 42);
      ColdFrame.Stubs.Set_Exception
        ("Stub_Test.Public.Get_Value", Stub_Test.Exception_Type'Identity, 2);
      ColdFrame.Stubs.Set_Exception
        ("Stub_Test.Public.Get_Value", Ada.Exceptions.Null_Id, 4);
      I := Stub_Test.Public.Get_Value;
      begin
         I := Stub_Test.Public.Get_Value;
         Assert (False, "should have raised exception (a)");
      exception
         when Stub_Test.Exception_Type => null;
      end;
      begin
         I := Stub_Test.Public.Get_Value;
         Assert (False, "should have raised exception (b)");
      exception
         when Stub_Test.Exception_Type => null;
      end;
      I := Stub_Test.Public.Get_Value;
   end Call_With_Exception;


   procedure Call_With_Out_Parameter
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Call_With_Out_Parameter
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      R : Stub_Test.Record_Type;
      use type Stub_Test.Record_Type;
   begin
      Set_Record_Type ("Stub_Test.Public.Retrieve_Record_Type",
                       "R",
                       (I => 42, F => 0.24));
      Set_Record_Type ("Stub_Test.Public.Retrieve_Record_Type",
                       "R",
                       (I => 24, F => 0.42),
                       3);
      R := (I => 0, F => 0.0);
      Stub_Test.Public.Retrieve_Record_Type (R);
      Assert (R = (I => 42, F => 0.24),
              "wrong value (a)");
      R := (I => 0, F => 0.0);
      Stub_Test.Public.Retrieve_Record_Type (R);
      Assert (R = (I => 42, F => 0.24),
              "wrong value (b)");
      R := (I => 0, F => 0.0);
      Stub_Test.Public.Retrieve_Record_Type (R);
      Assert (R = (I => 24, F => 0.42),
              "wrong value (c)");
      R := (I => 0, F => 0.0);
      Stub_Test.Public.Retrieve_Record_Type (R);
      Assert (R = (I => 24, F => 0.42),
              "wrong value (d)");
   end Call_With_Out_Parameter;


   procedure Call_With_In_Parameter_And_Return
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Call_With_In_Parameter_And_Return
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      R : Stub_Test.Record_Type;
      use type Stub_Test.Record_Type;
   begin
      Set_Record_Type ("Stub_Test.Public.Get_Record_Type",
                       "return",
                       (I => 42, F => 0.24));
      Set_Record_Type ("Stub_Test.Public.Get_Record_Type",
                       "return",
                       (I => 24, F => 0.42),
                       3);
      R := Stub_Test.Public.Get_Record_Type (False);
      Assert (not Get_Boolean ("Stub_Test.Public.Get_Record_Type", "B", 1),
              "wrong input (a)");
      Assert (R = (I => 42, F => 0.24),
              "wrong result (a)");
      R := Stub_Test.Public.Get_Record_Type (True);
      Assert (Get_Boolean ("Stub_Test.Public.Get_Record_Type", "B", 2),
              "wrong input (a)");
      Assert (R = (I => 42, F => 0.24),
              "wrong result (b)");
      R := Stub_Test.Public.Get_Record_Type (False);
      Assert (not Get_Boolean ("Stub_Test.Public.Get_Record_Type", "B", 3),
              "wrong input (c)");
      Assert (R = (I => 24, F => 0.42),
              "wrong result (c)");
      R := Stub_Test.Public.Get_Record_Type (True);
      Assert (Get_Boolean ("Stub_Test.Public.Get_Record_Type", "B", 4),
              "wrong input (d)");
      Assert (R = (I => 24, F => 0.42),
              "wrong value (d)");
   end Call_With_In_Parameter_And_Return;


   procedure Call_With_In_Parameter_And_Variant_Return
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Call_With_In_Parameter_And_Variant_Return
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, C);
      D : Stub_Test.Discriminated_Type;
      use type Stub_Test.Discriminated_Type;
      use type Stub_Test.Record_Type;
   begin
      Set_Discriminated_Type ("Stub_Test.Create_Discriminated",
                              "return",
                              (Discriminant => Stub_Test.I_T,
                               I => 42));
      Set_Discriminated_Type ("Stub_Test.Create_Discriminated",
                              "return",
                              (Discriminant => Stub_Test.R_T,
                               R => (I => 24, F => 0.42)),
                              3);
      D := Stub_Test.Create_Discriminated (False);
      Assert (not Get_Boolean ("Stub_Test.Create_Discriminated", "B", 1),
              "wrong input (a)");
      Assert (D = (Discriminant => Stub_Test.I_T,
                   I => 42),
              "wrong result (a)");
      D := Stub_Test.Create_Discriminated (True);
      Assert (Get_Boolean ("Stub_Test.Create_Discriminated", "B", 2),
              "wrong input (a)");
      Assert (D = (Discriminant => Stub_Test.I_T,
                   I => 42),
              "wrong result (b)");
      D := Stub_Test.Create_Discriminated (False);
      Assert (not Get_Boolean ("Stub_Test.Create_Discriminated", "B", 3),
              "wrong input (c)");
      Assert (D = (Discriminant => Stub_Test.R_T,
                   R => (I => 24, F => 0.42)),
              "wrong result (c)");
      D := Stub_Test.Create_Discriminated (True);
      Assert (Get_Boolean ("Stub_Test.Create_Discriminated", "B", 4),
              "wrong input (d)");
      Assert (D = (Discriminant => Stub_Test.R_T,
                   R => (I => 24, F => 0.42)),
              "wrong value (d)");
   end Call_With_In_Parameter_And_Variant_Return;


   type Case_1 is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (C : in out Case_1);

   function Name (C : Case_1) return Ada.Strings.Unbounded.String_Access;

   procedure Set_Up (C : in out Case_1);

   procedure Tear_Down (C :  in out Case_1);

   procedure Register_Tests (C : in out Case_1) is
   begin
      Register_Routine
        (C,
         Call_With_Return'Access,
         "function returns");
      Register_Routine
        (C,
         Call_With_Exception'Access,
         "exceptions");
      Register_Routine
        (C,
         Call_With_Out_Parameter'Access,
         "out parameters");
      Register_Routine
        (C,
         Call_With_In_Parameter_And_Return'Access,
         "functions with in parameters");
      Register_Routine
        (C,
         Call_With_In_Parameter_And_Variant_Return'Access,
         "functions with in parameters and variant returns");
   end Register_Tests;

   function Name (C : Case_1) return Ada.Strings.Unbounded.String_Access is
      pragma Warnings (Off, C);
   begin
      return new String'("Stub_Test tests");
   end Name;

   procedure Set_Up (C : in out Case_1) is
      pragma Warnings (Off, C);
   begin
      ColdFrame.Stubs.Set_Up;
      Stub_Test.Initialize;
   end Set_Up;

   procedure Tear_Down (C :  in out Case_1) is
      pragma Warnings (Off, C);
   begin
      ColdFrame.Stubs.Tear_Down;
      Stub_Test.Tear_Down;
   end Tear_Down;

   function Suite
     return AUnit.Test_Suites.Access_Test_Suite is
      use AUnit.Test_Suites;
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Case_1);
      return Result;
   end Suite;

end Stub_Test_Suite;
