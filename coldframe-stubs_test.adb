--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  $RCSfile: coldframe-stubs_test.adb,v $
--  $Revision: 23090fd5363e $
--  $Date: 2005/02/26 12:01:30 $
--  $Author: simon $

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with ColdFrame.Stubs;

procedure ColdFrame.Stubs_Test is

   procedure Generated_Stub_Procedure
     (Message : String;
      Input : Integer;
      Result : out Integer);

   procedure Generated_Stub_Procedure
     (Message : String;
      Input : Integer;
      Result : out Integer) is
      Occurrence : constant Positive
        := ColdFrame.Stubs.Note_Entry ("foo.bar.quux");
   begin
      String'Output
        (ColdFrame.Stubs.Get_Input_Value_Stream
           ("foo.bar.quux", "message", Occurrence, Message'Size),
         Message);
      Integer'Output
        (ColdFrame.Stubs.Get_Input_Value_Stream
           ("foo.bar.quux", "input", Occurrence, Input'Size),
         Input);
      ColdFrame.Stubs.Check_For_Exception
        ("foo.bar.quux", Occurrence);
      Result := Integer'Input
        (ColdFrame.Stubs.Get_Output_Value_Stream
           ("foo.bar.quux", "result", Occurrence));
   end Generated_Stub_Procedure;


   procedure Set_Output_Integer
   is new ColdFrame.Stubs.Set_Output_Value (Integer);
   function Get_Input_Integer
   is new ColdFrame.Stubs.Get_Input_Value (Integer);
   function Get_Input_String
   is new ColdFrame.Stubs.Get_Input_Value (String);

   Foo_Exception : exception;

begin

   ColdFrame.Stubs.Set_Up;

   declare
      Result : Integer;
   begin

      --  Setup
      Set_Output_Integer ("foo.bar.quux", "result", 42, 1);
      ColdFrame.Stubs.Set_Exception
        ("foo.bar.quux", Foo_Exception'Identity, 2);
      ColdFrame.Stubs.Set_Exception
        ("foo.bar.quux", Ada.Exceptions.Null_Id, 3);
      Set_Output_Integer ("foo.bar.quux", "result", 44, 3);
      Set_Output_Integer ("foo.bar.quux", "result", 45, 4);

      --  Check number of calls
      Put_Line
        ("calls => " & ColdFrame.Stubs.Number_Of_Calls ("foo.bar.quux")'Img);
      New_Line;

      --  First call
      Generated_Stub_Procedure ("first: input 24, result 42", 24, Result);
      Put_Line
        ("calls => " & ColdFrame.Stubs.Number_Of_Calls ("foo.bar.quux")'Img);
      Put_Line ("message => " & Get_Input_String
                  ("foo.bar.quux", "message", 1));
      Put_Line ("input => " & Get_Input_Integer
                  ("foo.bar.quux", "input", 1)'Img);
      Put_Line ("result => " & Result'Img);
      New_Line;

      --  Second call
      begin
         Generated_Stub_Procedure ("second: input 25, exception", 25, Result);
      exception
         when E : Foo_Exception =>
            Put_Line ("exception => " &
                        Ada.Exceptions.Exception_Information (E));
      end;
      Put_Line
        ("calls => " & ColdFrame.Stubs.Number_Of_Calls ("foo.bar.quux")'Img);
      Put_Line ("message => " & Get_Input_String
                  ("foo.bar.quux", "message", 2));
      Put_Line ("input => " & Get_Input_Integer
                  ("foo.bar.quux", "input", 2)'Img);
      New_Line;

      --  Third call
      Generated_Stub_Procedure ("third: input 26, result 44", 26, Result);
      Put_Line
        ("calls => " & ColdFrame.Stubs.Number_Of_Calls ("foo.bar.quux")'Img);
      Put_Line ("message => " & Get_Input_String
                  ("foo.bar.quux", "message", 3));
      Put_Line ("input => " & Get_Input_Integer
                  ("foo.bar.quux", "input", 3)'Img);
      Put_Line ("result => " & Result'Img);
      New_Line;

      --  Fourth call
      Generated_Stub_Procedure ("fourth and last: input 27, result 45",
                                27,
                                Result);
      Put_Line
        ("calls => " & ColdFrame.Stubs.Number_Of_Calls ("foo.bar.quux")'Img);
      Put_Line ("message => " & Get_Input_String
                  ("foo.bar.quux", "message", 4));
      Put_Line ("input => " & Get_Input_Integer
                  ("foo.bar.quux", "input", 4)'Img);
      Put_Line ("result => " & Result'Img);
      New_Line;

      --  Repeat the fourth call
      Generated_Stub_Procedure ("fourth and last: input 27, result 45",
                                27,
                                Result);
      Put_Line
        ("calls => " & ColdFrame.Stubs.Number_Of_Calls ("foo.bar.quux")'Img);
      Put_Line ("message => " & Get_Input_String
                  ("foo.bar.quux", "message", 4));
      Put_Line ("input => " & Get_Input_Integer
                  ("foo.bar.quux", "input", 4)'Img);
      Put_Line ("result => " & Result'Img);
      New_Line;

      --  Repeat the report
      Put_Line
        ("calls => " & ColdFrame.Stubs.Number_Of_Calls ("foo.bar.quux")'Img);
      Put_Line ("message => " & Get_Input_String
                  ("foo.bar.quux", "message", 4));
      Put_Line ("input => " & Get_Input_Integer
                  ("foo.bar.quux", "input", 4)'Img);
      Put_Line ("result => " & Result'Img);
      New_Line;

   end;

   ColdFrame.Stubs.Tear_Down;

end ColdFrame.Stubs_Test;

