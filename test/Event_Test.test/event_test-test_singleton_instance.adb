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

with Event_Test.Initialize;
with Event_Test.Tear_Down;

with Event_Test.Recipient;
with Event_Test.Events;

with ColdFrame.Project.Events;

package body Event_Test.Test_Singleton_Instance is

   --  An event will be delivered to its instance.
   procedure Simple_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Simple_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Mark (Recipient.Find);
      Inf : Recipient.Mark renames Recipient.Mark (Ev.all);
   begin
      Inf.Payload := (Ordinal => 2000,
                      Expected_At => ColdFrame.Project.Calendar.Clock);

      ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);

      Assert (R,
              Recipient.Get_Ordinal = 2000,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
   end Simple_Event;

   --  An event to self will be processed before other events.
   procedure Event_To_Self
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Event_To_Self
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Recipient.Self (Recipient.Find);
      Inf : Recipient.Self renames Recipient.Self (Ev.all);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Inf.Payload := (Ordinal => 2001,
                      Expected_At => ColdFrame.Project.Calendar.Clock);

      ColdFrame.Project.Events.Post (Ev, On => Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);

      Assert (R,
              Recipient.Get_Ordinal = 2002,
              "wrong ordinal" & Recipient.Get_Ordinal'Img);
   end Event_To_Self;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Simple_Event'Access, "Simple event");
      Registration.Register_Routine
        (T, Event_To_Self'Access, "Event to self");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String is
      pragma Warnings (Off, T);
   begin
      return new String'("Singleton instance events");
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

end Event_Test.Test_Singleton_Instance;
