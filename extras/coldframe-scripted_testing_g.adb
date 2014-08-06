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

with CArgv;
with Interfaces.C;
with Scripted_Testing;
with Tcl.Ada;

package body ColdFrame.Scripted_Testing_G is

   Dispatcher : Events.Event_Queue_P;

   procedure Register (The_Dispatcher : not null Events.Event_Queue_P)
   is
   begin
      Dispatcher := The_Dispatcher;
   end Register;

   ------------------------------------------------------------------------

   type Start_Dispatcher_Command
     is new Scripted_Testing.Command with null record;

   Start_Dispatcher_Name : constant String
     := "start_dispatcher";

   overriding
   function Tcl_Command
     (C      : access Start_Dispatcher_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Start_Dispatcher_Event
     is new Scripted_Testing.Event with null record;

   overriding
   procedure Execute (E : Start_Dispatcher_Event);

   function Tcl_Command
     (C      : access Start_Dispatcher_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Argv);
      use type Interfaces.C.int;
   begin
      if Argc /= 1 then
         Tcl.Ada.Tcl_AddErrorInfo
           (Interp,
            Start_Dispatcher_Name & " requires zero arguments");
         return Tcl.TCL_ERROR;
      end if;
      Scripted_Testing.Post
        (Start_Dispatcher_Event'(Scripted_Testing.Event with null record),
         Interp => Interp);
      return Tcl.TCL_OK;
   end Tcl_Command;

   procedure Execute (E : Start_Dispatcher_Event)
   is
      pragma Unreferenced (E);
   begin
      Dispatcher.Start;
   end Execute;

   The_Start_Dispatcher_Command :
     aliased Start_Dispatcher_Command;

   ------------------------------------------------------------------------

   type Wait_Until_Idle_Command
     is new Scripted_Testing.Command with null record;

   Wait_Until_Idle_Name : constant String
     := "wait_until_idle";

   overriding
   function Tcl_Command
     (C      : access Wait_Until_Idle_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Wait_Until_Idle_Event
     is new Scripted_Testing.Event with null record;

   overriding
   procedure Execute (E : Wait_Until_Idle_Event);

   function Tcl_Command
     (C      : access Wait_Until_Idle_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Argv);
      use type Interfaces.C.int;
   begin
      if Argc /= 1 then
         Tcl.Ada.Tcl_AddErrorInfo
           (Interp,
            Wait_Until_Idle_Name & " requires zero arguments");
         return Tcl.TCL_ERROR;
      end if;
      Scripted_Testing.Post
        (Wait_Until_Idle_Event'(Scripted_Testing.Event with null record),
         Interp => Interp);
      return Tcl.TCL_OK;
   end Tcl_Command;

   procedure Execute (E : Wait_Until_Idle_Event)
   is
      pragma Unreferenced (E);
   begin
      Dispatcher.Wait_Until_Idle (Ignoring_Timers => True);
   end Execute;

   The_Wait_Until_Idle_Command :
     aliased Wait_Until_Idle_Command;

   ------------------------------------------------------------------------

begin
   Scripted_Testing.Register
     (The_Command => The_Start_Dispatcher_Command'Access,
      To_Be_Named => Start_Dispatcher_Name);
   Scripted_Testing.Register
     (The_Command => The_Wait_Until_Idle_Command'Access,
      To_Be_Named => Wait_Until_Idle_Name);
end ColdFrame.Scripted_Testing_G;
