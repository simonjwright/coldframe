--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

with Ada.Exceptions;
with CArgv;
with ColdFrame.Stubs.Scripting;
with Digital_IO.Input_Signal_State_Callback;
with Digital_IO;
with Interfaces.C;
with Scripted_Testing;
with Tcl.Ada;

package body Digital_IO.Scripting is

   type Input_Signal_State_Callback_Command
     is new Scripted_Testing.Command with null record;

   Input_Signal_State_Callback_Name : constant String
     := "digital_io.input_signal_state_callback";

   overriding
   function Tcl_Command
     (C      : access Input_Signal_State_Callback_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Input_Signal_State_Callback_Event
     is new Scripted_Testing.Event with record
        Payload : Input_Signal_State;
     end record;

   overriding
   procedure Execute (E : Input_Signal_State_Callback_Event);

   function Tcl_Command
     (C      : access Input_Signal_State_Callback_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      use type Interfaces.C.int;
   begin
      if Argc /= 3 then
         Tcl.Ada.Tcl_AddErrorInfo
           (Interp,
            Input_Signal_State_Callback_Name & " requires 2 arguments");
         return Tcl.TCL_ERROR;
      end if;
      Scripted_Testing.Post
        (Input_Signal_State_Callback_Event'
           (Scripted_Testing.Event with
            Payload => (S => Input_Signal'Value (CArgv.Arg (Argv, 1)),
                        State => Boolean'Value (CArgv.Arg (Argv, 2)))),
         Interp => Interp);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Tcl.Ada.Tcl_AddErrorInfo
           (Interp,
            Input_Signal_State_Callback_Name
              & ": "
              & Ada.Exceptions.Exception_Message (E));
         return Tcl.TCL_ERROR;
   end Tcl_Command;

   procedure Execute (E : Input_Signal_State_Callback_Event)
   is
   begin
      Input_Signal_State_Callback.Call_Callbacks (E.Payload);
   end Execute;

   The_Input_Signal_State_Callback_Command :
     aliased Input_Signal_State_Callback_Command;

   ------------------------------------------------------------------------

   package Check_Output_Signal
     is new ColdFrame.Stubs.Scripting.Check_Passed_Value
       (Checked_Type      => Output_Signal,
        Checked_Type_Name => "digital_io.output_signal",
        Value             => Output_Signal'Value,
        Image             => Output_Signal'Image);
   pragma Unreferenced (Check_Output_Signal);

   package Check_Boolean
   is new ColdFrame.Stubs.Scripting.Check_Passed_Value
     (Checked_Type      => Boolean,
      Checked_Type_Name => "boolean",
      Value             => Boolean'Value,
      Image             => Boolean'Image);
   pragma Unreferenced (Check_Boolean);

   package Check_Boolean_For_Output_Signal
     is new ColdFrame.Stubs.Scripting.Check_Keyed_Value
       (Checked_Type      => Boolean,
        Checked_Type_Name => "boolean",
        Checked_Value     => Boolean'Value,
        Checked_Image     => Boolean'Image,
        Key_Type          => Output_Signal,
        Key_Type_Name     => "digital_io.output_signal",
        Key_Value         => Output_Signal'Value,
        Key_Image         => Output_Signal'Image);
   pragma Unreferenced (Check_Boolean_For_Output_Signal);

begin
   Scripted_Testing.Register
     (The_Command => The_Input_Signal_State_Callback_Command'Access,
      To_Be_Named => Input_Signal_State_Callback_Name);
end Digital_IO.Scripting;
