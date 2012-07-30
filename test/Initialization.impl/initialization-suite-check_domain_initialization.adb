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

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Initialization.Checking_Domain_Initialization;
with Initialization.Initialize;
with Initialization.Tear_Down;

separate (Initialization.Suite)
package body Check_Domain_Initialization is

   --  Check that the domain is in fact initialized when
   --  Domain_Initialized becomes True.
   procedure Check
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Check
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
   begin
      --  We have to allow time for the task to loop round and see the
      --  initialized value.
      delay 0.2;
      Assert (Checking_Domain_Initialization.Get_Initialized_As_Seen
                = True,
              "not properly initialized: "
                & Checking_Domain_Initialization.Get_Initialized_As_Seen'Img);
   end Check;


   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;
   function Name (T : Test_Case) return AUnit.Message_String;
   procedure Register_Tests (T : in out Test_Case);
   procedure Set_Up (T : in out Test_Case);
   procedure Tear_Down (T : in out Test_Case);

   function Name (T : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return new String'("Check domain initialization");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T,
         Check'Access,
         "Check Domain_Initialized");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Initialize;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Tear_Down;
   end Tear_Down;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      use AUnit.Test_Suites;
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Test_Case);
      return Result;
   end Suite;

end Check_Domain_Initialization;
