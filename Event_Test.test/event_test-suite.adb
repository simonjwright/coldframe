with Event_Test.Test_Engine;
with Event_Test.Test_Instance;
with Event_Test.Test_Singleton_Instance;
with Event_Test.Test_Class;

function Event_Test.Suite return AUnit.Test_Suites.Access_Test_Suite is
   use AUnit.Test_Suites;
   Result : Access_Test_Suite := new Test_Suite;
begin
   Add_Test (Result, new Test_Engine.Test_Case);
   Add_Test (Result, new Test_Instance.Test_Case);
   Add_Test (Result, new Test_Singleton_Instance.Test_Case);
   Add_Test (Result, new Test_Class.Test_Case);
   return Result;
end Event_Test.Suite;
