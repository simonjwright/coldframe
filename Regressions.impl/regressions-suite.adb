--  $Id: regressions-suite.adb,v ee4d0d6f3347 2003/10/31 06:34:47 simon $
--
--  Regression tests for ColdFrame.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
use AUnit.Test_Cases;
with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings (Off, Ada.Text_IO);
with ColdFrame.Project.Serialization;

with Regressions.Find_Active;
with Regressions.Find_Active_Singleton;
with Regressions.Initialize;
with Regressions.Tear_Down;
with Regressions.Serializable;

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


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Find_Active_Tests.Case_1);
      AUnit.Test_Suites.Add_Test (Result, new Serialization_Tests.Case_1);
      return Result;
   end Suite;

end Regressions.Suite;
