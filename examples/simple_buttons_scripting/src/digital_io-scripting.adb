--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

with ColdFrame.Callbacks.Scripting;
with ColdFrame.Stubs.Scripting;
with Digital_IO.Input_Signal_State_Callback;
with GNAT.String_Split;

package body Digital_IO.Scripting is

   function Input_Signal_State_Value (S : String) return Input_Signal_State;

   --  Digital_IO.Input_Signal_State_Callback is an instantiation of
   --  ColdFrame.Callbacks, so this adds the
   --  ColdFrame.Callbacks.Scripting extension.
   package Input_Signal_State_Callback
     is new Digital_IO.Input_Signal_State_Callback.Scripting
       (Callback_Type_Name => "digital_io.input_signal_state",
        Value              => Input_Signal_State_Value);
   pragma Unreferenced (Input_Signal_State_Callback);

   function Input_Signal_State_Value (S : String) return Input_Signal_State
   is
      Tokens : GNAT.String_Split.Slice_Set;
   begin
      GNAT.String_Split.Create (S => Tokens,
                                From => S,
                                Separators => " " & ASCII.HT,
                                Mode => GNAT.String_Split.Multiple);
      if Natural (GNAT.String_Split.Slice_Count (Tokens)) /= 2 then
         raise Constraint_Error
           with "digital_io.input_signal_state requires 2 components";
      end if;
      return Result : Input_Signal_State do
         Result.S :=
           Input_Signal'Value (GNAT.String_Split.Slice
                                 (Tokens,
                                  GNAT.String_Split.Slice_Number (1)));
         Result.State :=
           Boolean'Value (GNAT.String_Split.Slice
                            (Tokens,
                             GNAT.String_Split.Slice_Number (2)));
      end return;
   end Input_Signal_State_Value;

   ------------------------------------------------------------------------

   package Set_Boolean
   is new ColdFrame.Stubs.Scripting.Set_Returned_Value
     (Returned_Type      => Boolean,
      Returned_Type_Name => "boolean",
      Value              => Boolean'Value);
   pragma Unreferenced (Set_Boolean);

   ------------------------------------------------------------------------

   package Check_Output_Signal
     is new ColdFrame.Stubs.Scripting.Check_Passed_Value
       (Checked_Type      => Output_Signal,
        Checked_Type_Name => "digital_io.output_signal",
        Value             => Output_Signal'Value,
        Image             => Output_Signal'Image);
   pragma Unreferenced (Check_Output_Signal);

   ------------------------------------------------------------------------

   package Check_Boolean
   is new ColdFrame.Stubs.Scripting.Check_Passed_Value
     (Checked_Type      => Boolean,
      Checked_Type_Name => "boolean",
      Value             => Boolean'Value,
      Image             => Boolean'Image);
   pragma Unreferenced (Check_Boolean);

   ------------------------------------------------------------------------

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

end Digital_IO.Scripting;
