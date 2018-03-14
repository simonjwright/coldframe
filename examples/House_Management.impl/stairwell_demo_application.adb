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

--  Ensure no trampolines are generated.
pragma Restrictions (No_Implicit_Dynamic_Code);

with Ada.Exceptions;
with CArgv;
with Interfaces.C.Strings;
with Tcl.Ada;
with Tcl.Async;
with Tcl.Tk;

with ColdFrame.Project.Events.Standard.Trace;
with Digital_IO.Initialize;
with Digital_IO.Tcl.HCI;
with Digital_IO.Tcl.Initialize;
with House_Management.Initialize;

package body Stairwell_Demo_Application is

   --  Tell the application when a button is pushed/released.  The
   --  first argument is the button number and the optional second
   --  argument is the button state; if only one argument is passed,
   --  this procedure will simulate push then release.
   function Push_Button_Command
     (ClientData : in Integer;
      Interp : in Tcl.Tcl_Interp;
      Argc : in Interfaces.C.int;
      Argv : in CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;
   pragma Convention (C, Push_Button_Command);

   --  Handy wrapper for Interfaces.C.Strings.Free, so it can be used to free
   --  results.
   procedure Freeproc (BlockPtr : in Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, Freeproc);

   --  The ColdFrame event queue.
   Dispatcher : constant ColdFrame.Project.Events.Event_Queue_P
     := new ColdFrame.Project.Events.Standard.Trace.Event_Queue;

   function Init
     (Interp : in Tcl.Tcl_Interp) return Interfaces.C.int
   is
      package CreateCommands is new Tcl.Ada.Generic_Command (Integer);
      Command : Tcl.Tcl_Command;
      pragma Warnings (Off, Command);
      use type Interfaces.C.int;
   begin

      if Tcl.Tcl_Init (Interp) = Tcl.TCL_ERROR then
         return Tcl.TCL_ERROR;
      end if;

      if Tcl.Tk.Tk_Init (Interp) = Tcl.TCL_ERROR then
         return Tcl.TCL_ERROR;
      end if;

      Command := CreateCommands.Tcl_CreateCommand
        (Interp,
         "pushButton",
         Push_Button_Command'Access,
         0,
         null);

      --  To trace assignments to lampState, for Digital_IO.Tcl.Output
      Tcl.Async.Register (Interp);

      --  Now OK to kick off the ColdFrame side of things
      Digital_IO.Initialize (Dispatcher);
      Digital_IO.Tcl.Initialize (Dispatcher);
      House_Management.Initialize (Dispatcher);

      return Tcl.TCL_OK;

   end Init;

   function Push_Button_Command
     (ClientData : in Integer;
      Interp : in Tcl.Tcl_Interp;
      Argc : in Interfaces.C.int;
      Argv : in CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      Signal : Digital_IO.Input_Signal;
      pragma Warnings (Off, ClientData);
      use type Interfaces.C.int;
   begin
      pragma Assert
        (Argc = 2 or Argc = 3,
         "pushButton requires one or two arguments (button # [state])");
      Signal := Digital_IO.Input_Signal'Value
        (Interfaces.C.Strings.Value (CArgv.Argv_Pointer.Value (Argv) (1)));
      if Argc = 2 then
         Digital_IO.Tcl.HCI.Set_Input (Of_Signal => Signal, To => True);
         Digital_IO.Tcl.HCI.Set_Input (Of_Signal => Signal, To => False);
      else
         Digital_IO.Tcl.HCI.Set_Input
           (Of_Signal => Signal,
            To =>
              Interfaces.C.int'Value
                (Interfaces.C.Strings.Value
                   (CArgv.Argv_Pointer.Value (Argv) (2)))
                /= 0);
      end if;
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Tcl.Tcl_SetResult
           (Interp,
            Interfaces.C.Strings.New_String
              (Ada.Exceptions.Exception_Information (E)),
            Freeproc'Unrestricted_Access);
         return Tcl.TCL_ERROR;
   end Push_Button_Command;

   procedure Freeproc (BlockPtr : in Interfaces.C.Strings.chars_ptr)
   is
      Tmp : Interfaces.C.Strings.chars_ptr := BlockPtr;
   begin
      Interfaces.C.Strings.Free (Tmp);
   end Freeproc;

end Stairwell_Demo_Application;
