with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Event_Test.Test_Class is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Test_Case);

   function Name (T : Test_Case) return String_Access;

   procedure Set_Up (T : in out Test_Case);

   procedure Tear_Down (T :  in out Test_Case);

end Event_Test.Test_Class;
