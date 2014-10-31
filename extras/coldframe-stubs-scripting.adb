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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with CArgv;
with Interfaces.C;
with Scripted_Testing;
with Tcl.Ada;

package body ColdFrame.Stubs.Scripting is

   function "+"
     (S : String)
     return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "+"
     (U : Ada.Strings.Unbounded.Unbounded_String)
     return String
     renames Ada.Strings.Unbounded.To_String;


   package Check_Number_Of_Calls is
      procedure Initialize;
   end Check_Number_Of_Calls;

   package Save_Number_Of_Calls is
      procedure Initialize;
   end Save_Number_Of_Calls;

   package Check_Number_Of_New_Calls is
      procedure Initialize;
   end Check_Number_Of_New_Calls;


   package body Check_Number_Of_Calls is

      type Check_Command
        is new Scripted_Testing.Command with null record;

      Check_Name : constant String := "check_number_of_calls";

      overriding
      function Tcl_Command
        (C      : not null access Check_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

      type Check_Action
        is new Scripted_Testing.Action with record
           Subprogram : Ada.Strings.Unbounded.Unbounded_String;
           Expected   : Natural := 0;
        end record;

      overriding
      procedure Execute (A : Check_Action);

      function Tcl_Command
        (C      : not null access Check_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
      is
         pragma Unreferenced (C);
         use type Interfaces.C.int;
      begin
         --  The arguments are <subprogram> <number-of-calls>
         if Argc /= 3 then
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Check_Name & " requires 2 arguments");
            return Tcl.TCL_ERROR;
         end if;
         Scripted_Testing.Post
           (Check_Action'
              (Scripted_Testing.Action with
               Subprogram => +CArgv.Arg (Argv, 1),
               Expected   => Natural'Value (CArgv.Arg (Argv, 2))),
            Interp => Interp);
         return Tcl.TCL_OK;
      exception
         when E : others =>
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Check_Name & ": " & Ada.Exceptions.Exception_Message (E));
            return Tcl.TCL_ERROR;
      end Tcl_Command;

      procedure Execute (A : Check_Action)
      is
      begin
         declare
            Subprogram : constant String := +A.Subprogram;
            Actual : constant Natural :=
              Stubs.Number_Of_Calls (Subprogram);
         begin
            if Actual /= A.Expected then
               raise Scripted_Testing.Execution_Failure with
                 Subprogram
                 & " called"
                 & Natural'Image (Actual)
                 & " times, expected "
                 & Natural'Image (A.Expected);
            end if;
         end;
      exception
         when Ex : No_Subprogram | No_Parameter | No_Value =>
            raise Scripted_Testing.Execution_Failure with
              Ada.Exceptions.Exception_Message (Ex);
      end Execute;

      The_Check_Command :
        aliased Check_Command;

      procedure Initialize is
      begin
         Scripted_Testing.Register
           (The_Command => The_Check_Command'Access,
            To_Be_Named => Check_Name);
      end Initialize;

   end Check_Number_Of_Calls;


   package body Save_Number_Of_Calls is

      type Save_Command
        is new Scripted_Testing.Command with null record;

      Save_Name : constant String := "save_number_of_calls";

      overriding
      function Tcl_Command
        (C      : not null access Save_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

      type Save_Action
        is new Scripted_Testing.Action with record
           Subprogram : Ada.Strings.Unbounded.Unbounded_String;
        end record;

      overriding
      procedure Execute (A : Save_Action);

      function Tcl_Command
        (C      : not null access Save_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
      is
         pragma Unreferenced (C);
         use type Interfaces.C.int;
      begin
         --  The argument is <subprogram>
         if Argc /= 2 then
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Save_Name & " requires 1 argument");
            return Tcl.TCL_ERROR;
         end if;
         Scripted_Testing.Post
           (Save_Action'
              (Scripted_Testing.Action with
               Subprogram => +CArgv.Arg (Argv, 1)),
            Interp => Interp);
         return Tcl.TCL_OK;
      exception
         when E : others =>
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Save_Name & ": " & Ada.Exceptions.Exception_Message (E));
            return Tcl.TCL_ERROR;
      end Tcl_Command;

      procedure Execute (A : Save_Action)
      is
      begin
         Stubs.Save_Number_Of_Calls (+A.Subprogram);
      exception
         when Ex : No_Subprogram | No_Parameter | No_Value =>
            raise Scripted_Testing.Execution_Failure with
              Ada.Exceptions.Exception_Message (Ex);
      end Execute;

      The_Save_Command :
        aliased Save_Command;

      procedure Initialize is
      begin
         Scripted_Testing.Register
           (The_Command => The_Save_Command'Access,
            To_Be_Named => Save_Name);
      end Initialize;

   end Save_Number_Of_Calls;


   package body Check_Number_Of_New_Calls is

      type Check_Command
        is new Scripted_Testing.Command with null record;

      Check_Name : constant String := "check_number_of_new_calls";

      overriding
      function Tcl_Command
        (C      : not null access Check_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

      type Check_Action
        is new Scripted_Testing.Action with record
           Subprogram : Ada.Strings.Unbounded.Unbounded_String;
           Expected   : Natural := 0;
        end record;

      overriding
      procedure Execute (A : Check_Action);

      function Tcl_Command
        (C      : not null access Check_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
      is
         pragma Unreferenced (C);
         use type Interfaces.C.int;
      begin
         --  The arguments are <subprogram> <number-of-calls>
         if Argc /= 3 then
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Check_Name & " requires 2 arguments");
            return Tcl.TCL_ERROR;
         end if;
         Scripted_Testing.Post
           (Check_Action'
              (Scripted_Testing.Action with
               Subprogram => +CArgv.Arg (Argv, 1),
               Expected   => Natural'Value (CArgv.Arg (Argv, 2))),
            Interp => Interp);
         return Tcl.TCL_OK;
      exception
         when E : others =>
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Check_Name & ": " & Ada.Exceptions.Exception_Message (E));
            return Tcl.TCL_ERROR;
      end Tcl_Command;

      procedure Execute (A : Check_Action)
      is
      begin
         declare
            Subprogram : constant String := +A.Subprogram;
            Actual : constant Natural :=
              Stubs.Number_Of_New_Calls (Subprogram);
         begin
            if Actual /= A.Expected then
               raise Scripted_Testing.Execution_Failure with
                 Subprogram
                 & " called"
                 & Natural'Image (Actual)
                 & " times, expected "
                 & Natural'Image (A.Expected);
            end if;
         end;
      exception
         when Ex : No_Subprogram | No_Parameter | No_Value =>
            raise Scripted_Testing.Execution_Failure with
              Ada.Exceptions.Exception_Message (Ex);
      end Execute;

      The_Check_Command :
        aliased Check_Command;

      procedure Initialize is
      begin
         Scripted_Testing.Register
           (The_Command => The_Check_Command'Access,
            To_Be_Named => Check_Name);
      end Initialize;

   end Check_Number_Of_New_Calls;


   package body Set_Returned_Value is

      type Returned_Command
        is new Scripted_Testing.Command with null record;

      Returned_Name : constant String
        := "set-"
          & Ada.Strings.Fixed.Translate
            (Returned_Type_Name,
             Ada.Strings.Maps.Constants.Lower_Case_Map);

      overriding
      function Tcl_Command
        (C      : not null access Returned_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

      type Returned_Action
        is new Scripted_Testing.Action with record
           Subprogram : Ada.Strings.Unbounded.Unbounded_String;
           Parameter  : Ada.Strings.Unbounded.Unbounded_String;
           Returned   : Returned_Type;
           Call       : Integer := 0;
        end record;

      overriding
      procedure Execute (A : Returned_Action);

      function Tcl_Command
        (C      : not null access Returned_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
      is
         pragma Unreferenced (C);
         Call : Integer := 0;
         use type Interfaces.C.int;
      begin
         --  The arguments are <subprogram> <param> <value> {call}
         if not (Argc in 4 .. 5) then
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Returned_Name & " requires 3 or 4 arguments");
            return Tcl.TCL_ERROR;
         end if;
         if Argc = 5 then
            Call := Integer'Value (CArgv.Arg (Argv, 4));
         end if;
         Scripted_Testing.Post
           (Returned_Action'(Scripted_Testing.Action with
                             Subprogram => +CArgv.Arg (Argv, 1),
                             Parameter  => +CArgv.Arg (Argv, 2),
                             Returned   => Value (CArgv.Arg (Argv, 3)),
                             Call       => Call),
            Interp => Interp);
         return Tcl.TCL_OK;
      exception
         when E : others =>
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Returned_Name & ": " & Ada.Exceptions.Exception_Message (E));
            return Tcl.TCL_ERROR;
      end Tcl_Command;

      procedure Execute (A : Returned_Action)
      is
      begin
         declare
            procedure Set_Returned_Type
              is new ColdFrame.Stubs.Set_Output_Value (Returned_Type);
            Subprogram : constant String := +A.Subprogram;
            Parameter : constant String := +A.Parameter;
            Call : Natural := A.Call;
         begin
            if Call = 0 then
               Call := Number_Of_Calls (Subprogram) + 1;
            end if;
            Set_Returned_Type (Subprogram, Parameter, A.Returned, Call);
         end;
      exception
         when Ex : No_Subprogram | No_Parameter | Already_Set =>
            raise Scripted_Testing.Execution_Failure with
              Ada.Exceptions.Exception_Message (Ex);
      end Execute;

      The_Returned_Command :
        aliased Returned_Command;

      procedure Initialize is
      begin
         Scripted_Testing.Register
           (The_Command => The_Returned_Command'Access,
            To_Be_Named => Returned_Name);
      end Initialize;

   begin
      Initialize;
   end Set_Returned_Value;


   package body Check_Passed_Value is

      type Check_Command
        is new Scripted_Testing.Command with null record;

      Check_Name : constant String
        := "check-"
          & Ada.Strings.Fixed.Translate
            (Checked_Type_Name,
             Ada.Strings.Maps.Constants.Lower_Case_Map);

      overriding
      function Tcl_Command
        (C      : not null access Check_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

      type Check_Action
        is new Scripted_Testing.Action with record
           Subprogram : Ada.Strings.Unbounded.Unbounded_String;
           Parameter  : Ada.Strings.Unbounded.Unbounded_String;
           Expected   : Checked_Type;
           Call       : Integer := 0;
        end record;

      overriding
      procedure Execute (A : Check_Action);

      function Tcl_Command
        (C      : not null access Check_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
      is
         pragma Unreferenced (C);
         Call : Integer := 0;
         use type Interfaces.C.int;
      begin
         --  The arguments are <subprogram> <param> <value> {call}
         if not (Argc in 4 .. 5) then
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Check_Name & " requires 3 or 4 arguments");
            return Tcl.TCL_ERROR;
         end if;
         if Argc = 5 then
            Call := Integer'Value (CArgv.Arg (Argv, 4));
         end if;
         Scripted_Testing.Post
           (Check_Action'(Scripted_Testing.Action with
                          Subprogram => +CArgv.Arg (Argv, 1),
                          Parameter  => +CArgv.Arg (Argv, 2),
                          Expected   => Value (CArgv.Arg (Argv, 3)),
                          Call       => Call),
            Interp => Interp);
         return Tcl.TCL_OK;
      exception
         when E : others =>
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Check_Name & ": " & Ada.Exceptions.Exception_Message (E));
            return Tcl.TCL_ERROR;
      end Tcl_Command;

      procedure Execute (A : Check_Action)
      is
      begin
         declare
            function Get_Checked_Type
              is new ColdFrame.Stubs.Get_Input_Value (Checked_Type);
            Subprogram : constant String := +A.Subprogram;
            Parameter : constant String := +A.Parameter;
            V : constant Checked_Type :=
              Get_Checked_Type (Subprogram, Parameter, A.Call);
         begin
            if V /= A.Expected then
               raise Scripted_Testing.Execution_Failure with
                 Subprogram
                 & " ("
                 & Parameter
                 & ") expected "
                 & Image (A.Expected)
                 & ", got "
                 & Image (V);
            end if;
         end;
      exception
         when Ex : No_Subprogram | No_Parameter | No_Value =>
            raise Scripted_Testing.Execution_Failure with
              Ada.Exceptions.Exception_Message (Ex);
      end Execute;

      The_Check_Command :
        aliased Check_Command;

      procedure Initialize is
      begin
         Scripted_Testing.Register
           (The_Command => The_Check_Command'Access,
            To_Be_Named => Check_Name);
      end Initialize;

   begin
      Initialize;
   end Check_Passed_Value;


   package body Check_Keyed_Value is

      type Check_Command
        is new Scripted_Testing.Command with null record;

      Check_Name : constant String :=
        "check-"
        & Ada.Strings.Fixed.Translate
          (Checked_Type_Name,
           Ada.Strings.Maps.Constants.Lower_Case_Map)
        & "-for-"
        & Ada.Strings.Fixed.Translate
          (Key_Type_Name,
           Ada.Strings.Maps.Constants.Lower_Case_Map);

      overriding
      function Tcl_Command
        (C      : not null access Check_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

      type Check_Action
        is new Scripted_Testing.Action with record
           Subprogram        : Ada.Strings.Unbounded.Unbounded_String;
           Key_Parameter     : Ada.Strings.Unbounded.Unbounded_String;
           Key_Value         : Key_Type;
           Checked_Parameter : Ada.Strings.Unbounded.Unbounded_String;
           Expected          : Checked_Type;
        end record;

      overriding
      procedure Execute (A : Check_Action);

      function Tcl_Command
        (C      : not null access Check_Command;
         Interp : not null        Tcl.Tcl_Interp;
         Argc   :                 Interfaces.C.int;
         Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
      is
         pragma Unreferenced (C);
         use type Interfaces.C.int;
      begin
         --  The arguments are
         --  <subprogram> <key> <key-value> <param> <value>
         if Argc /= 6 then
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Check_Name & " requires 5 arguments");
            return Tcl.TCL_ERROR;
         end if;
         Scripted_Testing.Post
           (Check_Action'
              (Scripted_Testing.Action with
               Subprogram        => +CArgv.Arg (Argv, 1),
               Key_Parameter     => +CArgv.Arg (Argv, 2),
               Key_Value         => Key_Value (CArgv.Arg (Argv, 3)),
               Checked_Parameter => +CArgv.Arg (Argv, 4),
               Expected          => Checked_Value (CArgv.Arg (Argv, 5))),
            Interp                         => Interp);
         return Tcl.TCL_OK;
      exception
         when E : others =>
            Tcl.Ada.Tcl_AddErrorInfo
              (Interp,
               Check_Name & ": " & Ada.Exceptions.Exception_Message (E));
            return Tcl.TCL_ERROR;
      end Tcl_Command;

      procedure Execute (A : Check_Action)
      is
      begin
         declare
            function Get_Checked_Type
              is new ColdFrame.Stubs.Get_Keyed_Input_Value
                (Key_Type => Key_Type, Result_Type => Checked_Type);
            Subprogram : constant String := +A.Subprogram;
            Key_Parameter : constant String := +A.Key_Parameter;
            Checked_Parameter  : constant String := +A.Checked_Parameter;
            V : constant Checked_Type :=
              Get_Checked_Type (For_Subprogram_Named => Subprogram,
                                For_Parameter_Named  => Checked_Parameter,
                                When_Parameter_Named => Key_Parameter,
                                Had_Value            => A.Key_Value);
         begin
            if V /= A.Expected then
               raise Scripted_Testing.Execution_Failure with
                 Subprogram
                 & " ("
                 & Key_Parameter
                 & "=>"
                 & Ada.Strings.Fixed.Trim (Key_Image (A.Key_Value),
                                           Ada.Strings.Both)
                 & ","
                 & Checked_Parameter
                 & ") expected "
                 & Ada.Strings.Fixed.Trim (Checked_Image (A.Expected),
                                           Ada.Strings.Both)
                 & ", got "
                 & Checked_Image (V);
            end if;
         end;
      exception
         when Ex : No_Subprogram | No_Parameter | No_Value =>
            raise Scripted_Testing.Execution_Failure with
              Ada.Exceptions.Exception_Message (Ex);
      end Execute;

      The_Check_Command :
        aliased Check_Command;

      procedure Initialize is
      begin
         Scripted_Testing.Register
           (The_Command => The_Check_Command'Access,
            To_Be_Named => Check_Name);
      end Initialize;

   begin
      Initialize;
   end Check_Keyed_Value;


begin
   Check_Number_Of_Calls.Initialize;
   Save_Number_Of_Calls.Initialize;
   Check_Number_Of_New_Calls.Initialize;
end ColdFrame.Stubs.Scripting;
