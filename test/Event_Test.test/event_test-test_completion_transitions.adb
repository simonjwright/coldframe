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

with Event_Test.Completion_Transitions;
with Event_Test.Events;

with ColdFrame.Project.Events;

package body Event_Test.Test_Completion_Transitions is

   --  complete lifecycle
   procedure Complete_Lifecycle
      (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Complete_Lifecycle
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Assert (Completion_Transitions.Get_Status = 1,
              "incorrect status " & Completion_Transitions.Get_Status'Img);
      ColdFrame.Project.Events.Post
        (The_Event =>
           new Completion_Transitions.E2 (Completion_Transitions.Find),
         On => Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Completion_Transitions.Get_Status = 3,
              "incorrect status " & Completion_Transitions.Get_Status'Img);
   end Complete_Lifecycle;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Complete_Lifecycle'Access, "complete lifecycle");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String is
      pragma Warnings (Off, T);
   begin
      return new String'("Completion transitions");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Initialize;
   end Set_Up;

   procedure Tear_Down (T :  in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Tear_Down;
   end Tear_Down;

end Event_Test.Test_Completion_Transitions;
