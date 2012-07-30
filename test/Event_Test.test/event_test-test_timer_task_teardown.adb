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

with Event_Test.Initialize;
with Event_Test.Tear_Down;

with Event_Test.Timer_Task_Teardown;
with Event_Test.Events;

with ColdFrame.Project.Events;

package body Event_Test.Test_Timer_Task_Teardown is

   --  Just start the test; shouldn't actually fail
   procedure Run
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Run
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Timer_Task_Teardown.Start;
      delay 0.5;
      Assert (True, "shouldn't fail");
   end Run;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Run'Access, "running");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return new String'("Timer/task teardown");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Initialize;
   end Set_Up;

   procedure Tear_Down (T :  in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Tear_Down;
   end Tear_Down;

end Event_Test.Test_Timer_Task_Teardown;
