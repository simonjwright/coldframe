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

--  $RCSfile: stairwell_logging_demo.adb,v $
--  $Revision: 297981173790 $
--  $Date: 2014/03/08 16:34:09 $
--  $Author: simonjwright $

--  Derived from Terry Westley's TWAShell (Tcl Windowing Ada SHell).

--  This version of the Stairwell Lights example is intended to show
--  ColdFrame's facilities for logging the times required to process
--  events on an event queue (i.e., not synchronously-handled events.
--
--  For it to work, you must instantiate ColdFrame.Events_G to form
--  ColdFrame.Project.Events using
--  ColdFrame.Logging_Event_Basis.Event_Base. This design is
--  unsatisfactory to the extent that you have to rebuild the world
--  when you revert to the standard instntiation.
--
--  View the events using a browser at http://localhost:8080/events.

with Ada.Exceptions;
with CArgv;
with ColdFrame.Exceptions.Symbolic_Traceback;
with ColdFrame.Logging_Event_Basis.EWS_Support;
with ColdFrame.Project.Events.Standard;
with Digital_IO.HCI;
with Digital_IO.Initialize;
with EWS.Dynamic;
with EWS.Server;
with House_Management.Initialize;
with Interfaces.C.Strings;
with Tcl.Ada;
with Tcl.Async;
with Tcl.Tk;

pragma Warnings (Off, ColdFrame.Exceptions.Symbolic_Traceback);

procedure Stairwell_Logging_Demo is

   package C renames Interfaces.C;
   use type C.int;

   --  All the interfacing functions use convention C, so we have to
   --  have specifications (even if -gnaty didn't require them).

   --  Start the Tcl interpreter
   function Init (Interp : in Tcl.Tcl_Interp) return C.int;
   pragma Convention (C, Init);

   --  Tell the application we've pushed a button (argument is 0, 1
   --  etc).
   function Push_Button_Command
     (ClientData : in Integer;
      Interp : in Tcl.Tcl_Interp;
      Argc : in C.int;
      Argv : in CArgv.Chars_Ptr_Ptr) return C.int;
   pragma Convention (C, Push_Button_Command);

   --  Handy wrapper for C.Strings.Free, so it can be used to free
   --  results.
   procedure Freeproc (BlockPtr : in C.Strings.chars_ptr);
   pragma Convention (C, Freeproc);


   procedure Freeproc (BlockPtr : in C.Strings.chars_ptr) is
      Tmp : C.Strings.chars_ptr := BlockPtr;
   begin
      C.Strings.Free (Tmp);
   end Freeproc;


   function Init (Interp : in Tcl.Tcl_Interp) return C.int is
      package CreateCommands is new Tcl.Ada.Generic_Command (Integer);
      Command : Tcl.Tcl_Command;
      pragma Warnings (Off, Command);
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
         Push_Button_Command'Unrestricted_Access,
         0,
         null);

      --  To trace assignments to lampState, for Digital_IO.Output
      Tcl.Async.Register (Interp);

      return Tcl.TCL_OK;

   end Init;


   function Push_Button_Command
     (ClientData : in Integer;
      Interp : in Tcl.Tcl_Interp;
      Argc : in C.int;
      Argv : in CArgv.Chars_Ptr_Ptr) return C.int is
      Signal : Digital_IO.Signal_Name;
      pragma Warnings (Off, ClientData);
   begin
      pragma Assert (Argc = 2, "pushButton requires one argument (button #)");
      Signal := Digital_IO.Signal_Name'Value
        ("floor_" & C.Strings.Value (CArgv.Argv_Pointer.Value (Argv) (1)));
      Digital_IO.HCI.Set_Input (Of_Signal => Signal, To => True);
      Digital_IO.HCI.Set_Input (Of_Signal => Signal, To => False);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Tcl.Tcl_SetResult
           (Interp,
            C.Strings.New_String (Ada.Exceptions.Exception_Name (E)),
            Freeproc'Unrestricted_Access);
         return Tcl.TCL_ERROR;
   end Push_Button_Command;


   --  Argc and Argv include the command name
   Argc : C.int;
   Argv : CArgv.Chars_Ptr_Ptr;

   Dispatcher : constant ColdFrame.Project.Events.Event_Queue_P
     := new ColdFrame.Project.Events.Standard.Event_Queue;

begin

   EWS.Dynamic.Register
     (ColdFrame.Logging_Event_Basis.EWS_Support.Whole_Page'Access,
      "/events");
   EWS.Server.Serve (Using_Port => 8080);

   Digital_IO.Initialize (Dispatcher);
   House_Management.Initialize (Dispatcher);

   --  Get command-line arguments and put them into C-style "argv",
   --  as required by Tk_Main.
   CArgv.Create (Argc, Argv);

   --  Start Tcl
   Tcl.Tk.Tk_Main (Argc, Argv, Init'Unrestricted_Access);

end Stairwell_Logging_Demo;
