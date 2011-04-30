with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Initialization.Checking_Domain_Initialization;
with Initialization.Initialize;
with Initialization.Tear_Down;

separate (Initialization.Suite)
package body Check_Domain_Initialization is

   --  Check that the domain is in fact initialized when
   --  Domain_Initialized becomes True.
   procedure Check
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Check
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      --  We have to allow time for the task to loop round and see the
      --  initialized value.
      delay 0.2;
      Assert (Checking_Domain_Initialization.Get_Initialized_As_Seen
                = True,
              "not properly initialized: "
                & Checking_Domain_Initialization.Get_Initialized_As_Seen'Img);
   end Check;


   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;
   function Name (T : Test_Case) return String_Access;
   procedure Register_Tests (T : in out Test_Case);
   procedure Set_Up (T : in out Test_Case);
   procedure Tear_Down (T : in out Test_Case);

   function Name (T : Test_Case) return String_Access is
      pragma Unreferenced (T);
   begin
      return new String'("Check domain initialization");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T,
         Check'Access,
         "Check Domain_Initialized");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Initialize;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Tear_Down;
   end Tear_Down;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      use AUnit.Test_Suites;
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Test_Case);
      return Result;
   end Suite;

end Check_Domain_Initialization;
