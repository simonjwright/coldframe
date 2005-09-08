--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  $RCSfile: house_management-test_suite.ads,v $
--  $Revision: 9390212d4875 $
--  $Date: 2005/09/08 05:41:33 $
--  $Author: simonjwright $

with AUnit.Test_Suites;

package House_Management.Test_Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end House_Management.Test_Suite;
