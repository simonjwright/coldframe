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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Hierarchies.Test_Creations;
with Hierarchies.Test_Finds;
with Hierarchies.Test_Deletions;

function Hierarchies.Suite
  return AUnit.Test_Suites.Access_Test_Suite is
   use AUnit.Test_Suites;
   Result : constant Access_Test_Suite := new Test_Suite;
begin
   Add_Test (Result, new Test_Creations.Test_Case);
   Add_Test (Result, new Test_Finds.Test_Case);
   Add_Test (Result, new Test_Deletions.Test_Case);
   return Result;
end Hierarchies.Suite;
