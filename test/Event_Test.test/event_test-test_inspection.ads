with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Event_Test.Test_Inspection is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (C : in out Test_Case);

   function Name (C : Test_Case) return String_Access;

   procedure Set_Up (C : in out Test_Case);

   procedure Tear_Down (C :  in out Test_Case);

end Event_Test.Test_Inspection;
