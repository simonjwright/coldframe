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

with Event_Test.Machine;
with Event_Test.Machine.All_Instances;
with Event_Test.Machine.Vectors;
with Event_Test.Events;

with ColdFrame.Exceptions;
with ColdFrame.Project.Events;

package body Event_Test.Test_Instance is

   H : Machine.Handle;

   --  You can't post to self from outside the Queue
   procedure Post_To_Self
      (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Post_To_Self
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Machine.Mark (H);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      ColdFrame.Project.Events.Post_To_Self (Ev, On => Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (False, "no exception");
   exception
      when ColdFrame.Exceptions.Use_Error => null;
   end Post_To_Self;


   --  Delete as an action
   procedure Delete_As_Action
      (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_As_Action
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
      Ev1 : constant ColdFrame.Project.Events.Event_P
        := new Machine.Kill (H);
      Ev2 : constant ColdFrame.Project.Events.Event_P
        := new Machine.Kill (H);
   begin
      ColdFrame.Project.Events.Post (Ev1, On => Events.Dispatcher);
      ColdFrame.Project.Events.Post (Ev2, On => Events.Dispatcher);
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Machine.Vectors.Is_Empty (Machine.All_Instances),
              Machine.Vectors.Length (Machine.All_Instances)'Img &
              " instance(s) remaining");
   end Delete_As_Action;


   --  Delete as an action with running timer
   procedure Delete_As_Action_With_Timer
      (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_As_Action_With_Timer
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Machine.Kill (H);
   begin
      ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);
      Machine.Set_Timer (H, 2.5);
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Machine.Vectors.Is_Empty (Machine.All_Instances),
              Machine.Vectors.Length (Machine.All_Instances)'Img &
              " instance(s) remaining");
   end Delete_As_Action_With_Timer;


   --  Delete as an action with held events
   procedure Delete_As_Action_With_Held
      (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_As_Action_With_Held
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
      Ev1 : constant ColdFrame.Project.Events.Event_P
        := new Machine.Kill (H);
      Ev2 : constant ColdFrame.Project.Events.Event_P
        := new Machine.Kill (H);
      Ev3 : constant ColdFrame.Project.Events.Event_P
        := new Machine.Kill (H);
   begin
      ColdFrame.Project.Events.Post (Ev1,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 0.1);
      ColdFrame.Project.Events.Post (Ev2,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 0.1);
      ColdFrame.Project.Events.Post (Ev3,
                                     On => Events.Dispatcher,
                                     To_Fire_After => 0.2);
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Machine.Vectors.Is_Empty (Machine.All_Instances),
              Machine.Vectors.Length (Machine.All_Instances)'Img &
              " instance(s) remaining");
   end Delete_As_Action_With_Held;


   --  An event will be delivered to its instance.
   procedure Simple_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Simple_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Machine.Mark (H);
      Inf : Machine.Mark renames Machine.Mark (Ev.all);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 2000,
                      Expected_At => ColdFrame.Project.Calendar.Clock);
      ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Machine.Get_Ordinal (H) = 2000,
              "wrong ordinal" & Machine.Get_Ordinal (H)'Img);
   end Simple_Event;


   --  An event to self will be processed before other events.
   procedure Event_To_Self
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Event_To_Self
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Machine.Self (H);
      Inf : Machine.Self renames Machine.Self (Ev.all);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 2001,
                      Expected_At => ColdFrame.Project.Calendar.Clock);
      ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Machine.Get_Ordinal (H) = 2002,
              "wrong ordinal" & Machine.Get_Ordinal (H)'Img);
   end Event_To_Self;


   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Post_To_Self'Access, "Illegal posting to self");
      Registration.Register_Routine
        (T, Delete_As_Action'Access, "Delete as an action");
      Registration.Register_Routine
        (T,
         Delete_As_Action_With_Timer'Access,
         "Delete as an action (timeout event)");
      Registration.Register_Routine
        (T,
         Delete_As_Action_With_Held'Access,
         "Delete as an action (held event)");
      Registration.Register_Routine
        (T, Simple_Event'Access, "Simple event");
      Registration.Register_Routine
        (T, Event_To_Self'Access, "Event to self");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return new String'("Instance events");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Initialize;
      H := Machine.Create;
   end Set_Up;

   procedure Tear_Down (T :  in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Tear_Down;
      H := null;
   end Tear_Down;

end Event_Test.Test_Instance;
