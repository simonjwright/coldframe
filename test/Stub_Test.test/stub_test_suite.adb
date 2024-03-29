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
with AUnit.Test_Cases; use AUnit.Test_Cases;
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
   procedure Set_String
   is new ColdFrame.Stubs.Set_Output_Value (String);
   procedure Set_Fixed_Message
   is new ColdFrame.Stubs.Set_Output_Value (Stub_Test.Fixed_Message);
   procedure Set_Variable_Message
   is new ColdFrame.Stubs.Set_Output_Value (Stub_Test.Variable_Message);

   function Get_Boolean
   is new ColdFrame.Stubs.Get_Input_Value (Boolean);
   function Get_Integer
   is new ColdFrame.Stubs.Get_Input_Value (Integer);
   function Get_String
   is new ColdFrame.Stubs.Get_Input_Value (String);
   function Get_Fixed_Message
   is new ColdFrame.Stubs.Get_Input_Value (Stub_Test.Fixed_Message);
   function Get_Variable_Message
   is new ColdFrame.Stubs.Get_Input_Value (Stub_Test.Variable_Message);


   procedure Bad_Names
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Bad_Names
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      I : Integer;
      pragma Unreferenced (C);
      pragma Unreferenced (I);
   begin
      begin
         Set_Integer ("Stub_Test.Public.Get_Values", "return", 42, 2);
         Assert (False,
                 "should have raised exception (a)");
      exception
         when ColdFrame.Stubs.No_Subprogram => null;
      end;
      begin
         Set_Integer ("Stub_Test.Public.Get_Value", "result", 42, 2);
         Assert (False,
                 "should have raised exception (b)");
      exception
         when ColdFrame.Stubs.No_Parameter => null;
      end;
      begin
         I := Get_Integer ("Stub_Test.Public.Set_Values", "I", 1);
         Assert (False,
                 "should have raised exception (c)");
      exception
         when ColdFrame.Stubs.No_Subprogram => null;
      end;
      begin
         I := Get_Integer ("Stub_Test.Public.Set_Value", "J", 1);
         Assert (False,
                 "should have raised exception (d)");
      exception
         when ColdFrame.Stubs.No_Parameter => null;
      end;
      begin
         ColdFrame.Stubs.Set_Exception ("Stub_Test.Public.Get_Values",
                                        Constraint_Error'Identity,
                                        3);
         Assert (False,
                 "should have raised exception (e)");
      exception
         when ColdFrame.Stubs.No_Subprogram => null;
      end;
      begin
         I := ColdFrame.Stubs.Number_Of_Calls ("Stub_Test.Public.Get_Values");
         Assert (False,
                 "should have raised exception (f)");
      exception
         when ColdFrame.Stubs.No_Subprogram => null;
      end;
   end Bad_Names;


   procedure Overridden_Values
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Overridden_Values
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
      E, F, G, H : exception;
      I : Integer;
   begin
      Set_Integer ("Stub_Test.Public.Get_Value", "return", 42, 1);
      Set_Integer ("Stub_Test.Public.Get_Value", "return", 43, 2);
      ColdFrame.Stubs.Set_Exception
        ("Stub_Test.Public.Get_Value", E'Identity, 2);
      begin
         Set_Integer ("Stub_Test.Public.Get_Value", "return", 142, 1);
         Assert (False,
                 "should have raised exception (a)");
      exception
         when ColdFrame.Stubs.Already_Set => null;
      end;
      begin
         ColdFrame.Stubs.Set_Exception
           ("Stub_Test.Public.Get_Value", F'Identity, 2);
         Assert (False,
                 "should have raised exception (b)");
      exception
         when ColdFrame.Stubs.Already_Set => null;
      end;
      begin
         Set_Integer
           ("Stub_Test.Public.Get_Value", "return", 144, 3, Override => True);
         Assert (False,
                 "should have raised exception (c)");
      exception
         when ColdFrame.Stubs.Not_Already_Set => null;
      end;
      begin
         ColdFrame.Stubs.Set_Exception
           ("Stub_Test.Public.Get_Value", G'Identity, 1, Override => True);
         Assert (False,
                 "should have raised exception (d)");
      exception
         when ColdFrame.Stubs.Not_Already_Set => null;
      end;
      Set_Integer
        ("Stub_Test.Public.Get_Value", "return", 242, 1, Override => True);
      ColdFrame.Stubs.Set_Exception
        ("Stub_Test.Public.Get_Value", H'Identity, 2, Override => True);
      I := Stub_Test.Public.Get_Value;
      Assert (I = 242, "value not overridden; got " & I'Img);
      begin
         I := Stub_Test.Public.Get_Value;
         Assert (False,
                 "should have raised exception (e)");
      exception
         when H => null;
      end;
   end Overridden_Values;


   procedure Missing_Values
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Missing_Values
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
      I : Integer;
   begin
      Set_Integer ("Stub_Test.Public.Get_Value", "return", 42, 2);
      begin
         I := Stub_Test.Public.Get_Value;
         Assert (False, "should have raised exception (a)");
      exception
         when ColdFrame.Stubs.No_Value => null;
      end;
      Stub_Test.Public.Set_Value (24);
      I := Get_Integer ("Stub_Test.Public.Set_Value", "I", 1);
      Assert (I = 24, "wrong value (a)");
      begin
         I := Get_Integer ("Stub_Test.Public.Set_Value", "I", 2);
         Assert (False, "should have raised exception (b)");
      exception
         when ColdFrame.Stubs.No_Value => null;
      end;
      begin
         declare
            S : constant String :=
              Get_String ("Stub_Test.Public.Process_String", "S", 1);
            pragma Unreferenced (S);
         begin
            Assert (False, "should have raised exception (c)");
         end;
      exception
         when ColdFrame.Stubs.No_Value => null;
      end;
   end Missing_Values;


   procedure Input_Values
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Input_Values
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
      Retrieved : Integer;
   begin
      begin
         Retrieved :=
           Get_Integer ("Stub_Test.Public.Set_Value", "I", 0);
         Assert (False, "should have raised exception, call 0");
      exception
         when ColdFrame.Stubs.No_Value => null;
      end;
      for I in 1 .. 10 loop
         Stub_Test.Public.Set_Value (I);
      end loop;
      begin
         Retrieved :=
           Get_Integer ("Stub_Test.Public.Set_Value", "I", 11);
         Assert (False, "should have raised exception, call 11");
      exception
         when ColdFrame.Stubs.No_Value => null;
      end;
      begin
         Retrieved :=
           Get_Integer ("Stub_Test.Public.Set_Value", "I", -10);
         Assert (False, "should have raised exception, call -10");
      exception
         when ColdFrame.Stubs.No_Value => null;
      end;
      for I in 1 .. 10 loop
         Retrieved := Get_Integer ("Stub_Test.Public.Set_Value", "I", I);
         Assert (Retrieved = I,
                 "wrong value " & Retrieved'Img & " for call " & I'Img);
      end loop;
      Retrieved :=
        Get_Integer ("Stub_Test.Public.Set_Value", "I", 0);
      Assert (Retrieved = 10,
              "wrong value " & Retrieved'Img & " for call 0");
      for I in reverse -9 .. -1 loop
         Retrieved := Get_Integer ("Stub_Test.Public.Set_Value", "I", I);
         Assert (Retrieved = 10 + I,
                 "wrong value " & Retrieved'Img & " for call " & I'Img);
      end loop;
   end Input_Values;


   procedure Call_With_Return
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Call_With_Return
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
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
      pragma Unreferenced (C);
      I : Integer;
      pragma Unreferenced (I);
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
      pragma Unreferenced (C);
      R : Stub_Test.Record_Type;
      pragma Warnings (Off, R);
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
      pragma Unreferenced (C);
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
      Assert (not Get_Boolean
                ("Stub_Test.Public.Get_Record_Type",
                 "B"),
              "wrong input (c')");
      R := Stub_Test.Public.Get_Record_Type (True);
      Assert (Get_Boolean ("Stub_Test.Public.Get_Record_Type", "B", 4),
              "wrong input (d)");
      Assert (R = (I => 24, F => 0.42),
              "wrong value (d)");
      Assert (Get_Boolean
                ("Stub_Test.Public.Get_Record_Type",
                 "B"),
              "wrong input (d')");
   end Call_With_In_Parameter_And_Return;


   procedure Call_With_In_Parameter_And_Variant_Return
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Call_With_In_Parameter_And_Variant_Return
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
      D : Stub_Test.Discriminated_Type;
      use type Stub_Test.Discriminated_Type;
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


   procedure Call_With_String
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Call_With_String
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
   begin
      Set_String ("Stub_Test.Public.Process_String",
                  "return",
                  (1 .. 80 => 'a', 81 .. 160 =>  'b', 161 .. 240 =>  'c'));
      declare
         S : constant String :=
           Stub_Test.Public.Process_String
           ((1 .. 80 => 'z', 81 .. 160 =>  'y', 161 .. 240 =>  'x'));
      begin
         Assert (S = (1 .. 80 => 'a', 81 .. 160 =>  'b', 161 .. 240 =>  'c'),
                 "wrong string returned");
         Assert (Get_String ("Stub_Test.Public.Process_String", "S")
                   = (1 .. 80 => 'z', 81 .. 160 =>  'y', 161 .. 240 =>  'x'),
                 "wrong string passed");
      end;
   end Call_With_String;


   procedure Call_With_Fixed_Message
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Call_With_Fixed_Message
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
   begin
      Set_Fixed_Message ("Stub_Test.Public.Process_Fixed_Message",
                         "return",
                         ("wxyz"));
      declare
         S : constant Stub_Test.Fixed_Message :=
           Stub_Test.Public.Process_Fixed_Message ("abcd");
      begin
         Assert (S = "wxyz",
                 "wrong fixed_message returned");
         Assert (Get_Fixed_Message
                   ("Stub_Test.Public.Process_Fixed_Message", "M")
                   = "abcd",
                 "wrong fixed_message passed");
      end;
   end Call_With_Fixed_Message;


   procedure Call_With_Variable_Message
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Call_With_Variable_Message
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
      use type Stub_Test.Variable_Message;
      use Stub_Test.Variable_Message_Package;
   begin
      Set_Variable_Message ("Stub_Test.Public.Process_Variable_Message",
                            "return",
                            To_Bounded_String ("wxyz"));
      declare
         S : constant Stub_Test.Variable_Message :=
           Stub_Test.Public.Process_Variable_Message
           (To_Bounded_String ("abcd"));
      begin
         Assert (S = To_Bounded_String ("wxyz"),
                 "wrong variable_message returned");
         Assert (Get_Variable_Message
                   ("Stub_Test.Public.Process_Variable_Message", "M")
                   = To_Bounded_String ("abcd"),
                 "wrong variable_message passed");
      end;
   end Call_With_Variable_Message;


   procedure Call_With_Keyed_Access
     (C : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Call_With_Keyed_Access
     (C : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (C);
      function Get_Character_For_Integer
        is new ColdFrame.Stubs.Get_Keyed_Input_Value
          (Key_Type    => Integer,
           Result_Type => Character);
      Ch : Character;
   begin

      --  No subprogram
      begin
         Ch :=
           Get_Character_For_Integer
             (For_Subprogram_Named => "Stub_Test.Public.Set_Keyed_ValueX",
              For_Parameter_Named => "Value",
              When_Parameter_Named => "Key",
              Had_Value => 42);
         Assert (False, "should have raised No_Subprogram");
      exception
         when ColdFrame.Stubs.No_Subprogram => null;
      end;

      --  No calls
      begin
         Ch :=
           Get_Character_For_Integer
             (For_Subprogram_Named => "Stub_Test.Public.Set_Keyed_Value",
              For_Parameter_Named => "Value",
              When_Parameter_Named => "Key",
              Had_Value => 42);
         Assert (False, "should have raised No_Value (no calls)");
      exception
         when ColdFrame.Stubs.No_Value => null;
      end;

      --  Data
      Stub_Test.Public.Set_Keyed_Value (Key => 42, Value => 'a');
      Stub_Test.Public.Set_Keyed_Value (Key => 43, Value => 'b');
      Stub_Test.Public.Set_Keyed_Value (Key => 42, Value => 'c');
      Stub_Test.Public.Set_Keyed_Value (Key => 44, Value => 'd');

      --  Invalid parameter name (for)
      begin
         Ch :=
           Get_Character_For_Integer
             (For_Subprogram_Named => "Stub_Test.Public.Set_Keyed_Value",
              For_Parameter_Named => "ValueX",
              When_Parameter_Named => "Key",
              Had_Value => 42);
         Assert (False, "should have raised No_Parameter (for)");
      exception
         when ColdFrame.Stubs.No_Parameter => null;
      end;

      --  Invalid parameter name (when)
      begin
         Ch :=
           Get_Character_For_Integer
             (For_Subprogram_Named => "Stub_Test.Public.Set_Keyed_Value",
              For_Parameter_Named => "Value",
              When_Parameter_Named => "KeyX",
              Had_Value => 42);
         Assert (False, "should have raised No_Parameter (when)");
      exception
         when ColdFrame.Stubs.No_Parameter => null;
      end;

      --  Valid calls
      Ch :=
        Get_Character_For_Integer
          (For_Subprogram_Named => "Stub_Test.Public.Set_Keyed_Value",
           For_Parameter_Named => "Value",
           When_Parameter_Named => "Key",
           Had_Value => 42);
      Assert (Ch = 'c',
              Ch & " returned, expecting c");
      Ch :=
        Get_Character_For_Integer
          (For_Subprogram_Named => "Stub_Test.Public.Set_Keyed_Value",
           For_Parameter_Named => "Value",
           When_Parameter_Named => "Key",
           Had_Value => 43);
      Assert (Ch = 'b',
              Ch & " returned, expecting b");
      Ch :=
        Get_Character_For_Integer
          (For_Subprogram_Named => "Stub_Test.Public.Set_Keyed_Value",
           For_Parameter_Named => "Value",
           When_Parameter_Named => "Key",
           Had_Value => 44);
      Assert (Ch = 'd',
              Ch & " returned, expecting d");

      --  No key
      begin
         Ch :=
           Get_Character_For_Integer
             (For_Subprogram_Named => "Stub_Test.Public.Set_Keyed_Value",
              For_Parameter_Named => "Value",
              When_Parameter_Named => "Key",
              Had_Value => 46);
         Assert (False, "should have raised No_Value (key=46)");
      exception
         when ColdFrame.Stubs.No_Value => null;
      end;

   end Call_With_Keyed_Access;


   type Case_1 is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (C : in out Case_1);

   function Name (C : Case_1) return AUnit.Message_String;

   procedure Set_Up (C : in out Case_1);

   procedure Tear_Down (C :  in out Case_1);

   procedure Register_Tests (C : in out Case_1) is
   begin
      Registration.Register_Routine
        (C,
         Bad_Names'Access,
         "bad names");
      Registration.Register_Routine
        (C,
         Overridden_Values'Access,
         "overridden values");
      Registration.Register_Routine
        (C,
         Missing_Values'Access,
         "missing values");
      Registration.Register_Routine
        (C,
         Input_Values'Access,
         "input values");
      Registration.Register_Routine
        (C,
         Call_With_Return'Access,
         "function returns");
      Registration.Register_Routine
        (C,
         Call_With_Exception'Access,
         "exceptions");
      Registration.Register_Routine
        (C,
         Call_With_Out_Parameter'Access,
         "out parameters");
      Registration.Register_Routine
        (C,
         Call_With_In_Parameter_And_Return'Access,
         "functions with in parameters");
      Registration.Register_Routine
        (C,
         Call_With_In_Parameter_And_Variant_Return'Access,
         "functions with in parameters and variant returns");
      Registration.Register_Routine
        (C,
         Call_With_String'Access,
         "strings");
      Registration.Register_Routine
        (C,
         Call_With_Fixed_Message'Access,
         "fixed strings");
      Registration.Register_Routine
        (C,
         Call_With_Variable_Message'Access,
         "variable strings");
      Registration.Register_Routine
        (C,
         Call_With_Keyed_Access'Access,
         "keyed access");
   end Register_Tests;

   function Name (C : Case_1) return AUnit.Message_String is
      pragma Unreferenced (C);
   begin
      return new String'("Stub_Test tests");
   end Name;

   procedure Set_Up (C : in out Case_1) is
      pragma Unreferenced (C);
   begin
      ColdFrame.Stubs.Set_Up;
      Stub_Test.Initialize;
   end Set_Up;

   procedure Tear_Down (C :  in out Case_1) is
      pragma Unreferenced (C);
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
