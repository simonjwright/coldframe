with Hierarchies.Test_Creations;
with Hierarchies.Test_Finds;
with Hierarchies.Test_Deletions;

function Hierarchies.Suite return AUnit.Test_Suites.Access_Test_Suite is
   use AUnit.Test_Suites;
   Result : Access_Test_Suite := new Test_Suite;
begin
   Add_Test (Result, new Test_Creations.Test_Case);
   Add_Test (Result, new Test_Finds.Test_Case);
   Add_Test (Result, new Test_Deletions.Test_Case);
   return Result;
end Hierarchies.Suite;
