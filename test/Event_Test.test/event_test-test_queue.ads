with AUnit.Test_Cases; use AUnit.Test_Cases;

package Event_Test.Test_Queue is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Test_Case);

   function Name (T : Test_Case) return AUnit.Message_String;

end Event_Test.Test_Queue;
