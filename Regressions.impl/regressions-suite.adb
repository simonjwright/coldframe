with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
use AUnit.Test_Cases;
with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings (Off, Ada.Text_IO);

with Regressions.Find_Active;
with Regressions.Initialize;
with Regressions.Tear_Down;

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


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Find_Active_Tests.Case_1);
      return Result;
   end Suite;

end Regressions.Suite;
