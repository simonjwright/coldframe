--  $Id: stairwell_demo.adb,v a3d067f20749 2003/01/07 20:58:36 simon $
--  Derived from Terry Westley's TWAShell (Tcl Windowing Ada SHell).

with Ada.Exceptions;
with CArgv;
with Interfaces.C.Strings;
with Tcl.Ada;
with Tcl.Tk;

with Digital_IO.Initialize;
with Digital_IO.HCI;
with House_Management.Initialize;

with ColdFrame.Exceptions.Symbolic_Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Symbolic_Traceback);

procedure Stairwell_Demo is

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

   --  Find whether the lamp specified in argv (a, b etc) is set or
   --  not.
   function Get_Lamp_State_Command
     (ClientData : in Integer;
      Interp : in Tcl.Tcl_Interp;
      Argc : in C.int;
      Argv : in CArgv.Chars_Ptr_Ptr) return C.int;
   pragma Convention (C, Get_Lamp_State_Command);

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
   begin

      if Tcl.Tcl_Init (Interp) = Tcl.TCL_ERROR then
         return Tcl.TCL_ERROR;
      end if;

      if Tcl.Tk.Tk_Init (Interp) = Tcl.TCL_ERROR then
         return Tcl.TCL_ERROR;
      end if;

      Tcl.Ada.Tcl_StaticPackage (Interp,
                                 "Tk",
                                 Tcl.Tk.Tk_Init'Access,
                                 Tcl.Tk.Tk_SafeInit'Access);

      Command := CreateCommands.Tcl_CreateCommand
        (Interp,
         "pushButton",
         Push_Button_Command'Unrestricted_Access,
         0,
         null);

      Command := CreateCommands.Tcl_CreateCommand
        (Interp,
         "getLampState",
         Get_Lamp_State_Command'Unrestricted_Access,
         0,
         null);

      return Tcl.TCL_OK;

   end Init;


   function Get_Lamp_State_Command
     (ClientData : in Integer;
      Interp : in Tcl.Tcl_Interp;
      Argc : in C.int;
      Argv : in CArgv.Chars_Ptr_Ptr) return C.int is
      Signal : Digital_IO.Signal_Name;
      State : Boolean;
   begin
      pragma Assert
        (Argc = 2, "getLampState requires one argument (lamp letter)");
      Signal := Digital_IO.Signal_Name'Value
        ("lamp_" & C.Strings.Value (CArgv.Argv_Pointer.Value (Argv) (1)));
      State := Digital_IO.HCI.Get_State (Of_Signal => Signal);
      Tcl.Tcl_SetResult (Interp,
                         C.Strings.New_String (State'Img),
                         Freeproc'Unrestricted_Access);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Tcl.Tcl_SetResult
           (Interp,
            C.Strings.New_String (Ada.Exceptions.Exception_Name (E)),
            Freeproc'Unrestricted_Access);
         return Tcl.TCL_ERROR;
   end Get_Lamp_State_Command;


   function Push_Button_Command
     (ClientData : in Integer;
      Interp : in Tcl.Tcl_Interp;
      Argc : in C.int;
      Argv : in CArgv.Chars_Ptr_Ptr) return C.int is
      Signal : Digital_IO.Signal_Name;
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

begin

   Digital_IO.Initialize;
   House_Management.Initialize;

   --  Get command-line arguments and put them into C-style "argv",
   --  as required by Tk_Main.
   CArgv.Create (Argc, Argv);

   --  Start Tcl
   Tcl.Tk.Tk_Main (Argc, Argv, Init'Unrestricted_Access);

end Stairwell_Demo;
