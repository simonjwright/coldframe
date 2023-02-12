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
with ColdFrame.Stubs;
with Interfaces.C;
with Scripted_Testing;
with Tcl.Ada;

package body ColdFrame.Scripted_Testing_G is

   Dispatcher   : Events.Event_Queue_P;
   Initializing : Queue_Procedure_Access;

   procedure Register (The_Dispatcher  : not null Events.Event_Queue_P;
                       With_Initialize : not null Queue_Procedure_Access)
   is
   begin
      Dispatcher   := The_Dispatcher;
      Initializing := With_Initialize;
   end Register;

   ------------------------------------------------------------------------

   type Initialize_Command
     is new Scripted_Testing.Command with null record;

   Initialize_Name : constant String
     := "initialize";

   overriding
   function Tcl_Command
     (C      : not null access Initialize_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Initialize_Action
     is new Scripted_Testing.Action with null record;

   overriding
   procedure Execute (A : Initialize_Action);

   function Tcl_Command
     (C      : not null access Initialize_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Argv);
      use type Interfaces.C.int;
   begin
      if Argc /= 1 then
         Tcl.Ada.Tcl_AddErrorInfo
           (Interp,
            Initialize_Name & " requires zero arguments");
         return Tcl.TCL_ERROR;
      end if;
      Scripted_Testing.Post
        (Initialize_Action'(Scripted_Testing.Action with null record),
         Interp => Interp);
      return Tcl.TCL_OK;
   end Tcl_Command;

   procedure Execute (A : Initialize_Action)
   is
      pragma Unreferenced (A);
   begin
      Initializing (Dispatcher);
   end Execute;

   The_Initialize_Command :
     aliased Initialize_Command;

   ------------------------------------------------------------------------

   type Start_Dispatcher_Command
     is new Scripted_Testing.Command with null record;

   Start_Dispatcher_Name : constant String
     := "start_dispatcher";

   overriding
   function Tcl_Command
     (C      : not null access Start_Dispatcher_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Start_Dispatcher_Action
     is new Scripted_Testing.Action with null record;

   overriding
   procedure Execute (A : Start_Dispatcher_Action);

   function Tcl_Command
     (C      : not null access Start_Dispatcher_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
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
        (Start_Dispatcher_Action'(Scripted_Testing.Action with null record),
         Interp => Interp);
      return Tcl.TCL_OK;
   end Tcl_Command;

   procedure Execute (A : Start_Dispatcher_Action)
   is
      pragma Unreferenced (A);
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
     (C      : not null access Wait_Until_Idle_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Wait_Until_Idle_Action
     is new Scripted_Testing.Action with null record;

   overriding
   procedure Execute (A : Wait_Until_Idle_Action);

   function Tcl_Command
     (C      : not null access Wait_Until_Idle_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
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
        (Wait_Until_Idle_Action'(Scripted_Testing.Action with null record),
         Interp => Interp);
      return Tcl.TCL_OK;
   end Tcl_Command;

   procedure Execute (A : Wait_Until_Idle_Action)
   is
      pragma Unreferenced (A);
   begin
      Dispatcher.Wait_Until_Idle (Ignoring_Timers => True);
   end Execute;

   The_Wait_Until_Idle_Command :
     aliased Wait_Until_Idle_Command;

   ------------------------------------------------------------------------

begin

   Scripted_Testing.Register
     (The_Command => The_Initialize_Command'Access,
      To_Be_Named => Initialize_Name);
   Scripted_Testing.Register
     (The_Command => The_Start_Dispatcher_Command'Access,
      To_Be_Named => Start_Dispatcher_Name);
   Scripted_Testing.Register
     (The_Command => The_Wait_Until_Idle_Command'Access,
      To_Be_Named => Wait_Until_Idle_Name);

   ColdFrame.Stubs.Set_Up;
end ColdFrame.Scripted_Testing_G;
