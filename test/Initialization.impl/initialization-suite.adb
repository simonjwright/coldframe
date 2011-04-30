package body Initialization.Suite is

   package Check_Domain_Initialization is
      function Suite return AUnit.Test_Suites.Access_Test_Suite;
   end Check_Domain_Initialization;
   package body Check_Domain_Initialization is separate;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      use AUnit.Test_Suites;
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, Check_Domain_Initialization.Suite);
      return Result;
   end Suite;

end Initialization.Suite;
