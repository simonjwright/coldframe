--  $Id: regressions-suite.ads,v 6e06cee2bcf0 2003/10/31 06:34:19 simon $
--
--  Regression tests for ColdFrame.

with AUnit.Test_Suites;

package Regressions.Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Regressions.Suite;
