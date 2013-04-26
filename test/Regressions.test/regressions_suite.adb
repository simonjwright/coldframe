--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  Regression tests for ColdFrame.

--  Units that need testing

with SF_2991721.Test;
with SF_3086637_Suite;

--  Call up units that only have to compile

pragma Warnings (Off);
--  with Compilation_Regressions.Subunits;
with Ignored_Child_Packages;
pragma Warnings (On);

package body Regressions_Suite is


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new SF_2991721.Test.Case_1);
      AUnit.Test_Suites.Add_Test (Result, SF_3086637_Suite);
      return Result;
   end Suite;


end Regressions_Suite;
