--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  $RCSfile: coldframe-stubs_test.adb,v $
--  $Revision: 5e5d12b869da $
--  $Date: 2005/02/23 23:11:25 $
--  $Author: simon $

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with ColdFrame.Test_Stub_Support;

procedure ColdFrame.Test_Stub_Support_Test is

   procedure Generated_Stub_Procedure
     (Input : Integer;
      Result : out Integer);

   procedure Generated_Stub_Procedure
     (Input : Integer;
      Result : out Integer) is
      Occurrence : constant Positive
        := ColdFrame.Test_Stub_Support.Note_Entry ("foo.bar.quux");
   begin
      Integer'Output
        (ColdFrame.Test_Stub_Support.Get_Input_Value_Stream
           ("foo.bar.quux", "input", Occurrence),
         Input);
      ColdFrame.Test_Stub_Support.Check_For_Exception
        ("foo.bar.quux", Occurrence);
      Result := Integer'Input
        (ColdFrame.Test_Stub_Support.Get_Output_Value_Stream
           ("foo.bar.quux", "result", Occurrence));
   end Generated_Stub_Procedure;


   procedure Set_Quux_Integer
   is new ColdFrame.Test_Stub_Support.Set_Output_Value
     (Integer, "foo.bar.quux");
--     procedure Set_Quux_Exception
--     is new ColdFrame.Test_Stub_Support.Set_Output_Value
--       (Ada.Exceptions.Exception_Id, "foo.bar.quux");
   function Get_Quux_Integer
   is new ColdFrame.Test_Stub_Support.Get_Input_Value
     (Integer, "foo.bar.quux");

   Foo_Exception : exception;

begin

   ColdFrame.Test_Stub_Support.Set_Up;

   declare
      Result : Integer;
   begin
      Set_Quux_Integer ("result", 42);
--        Set_Quux_Exception ("exception", Foo_Exception'Identity);
      Generated_Stub_Procedure (24, Result);
      Put_Line (Result'Img);                         --  should be 42
      Put_Line (Get_Quux_Integer ("input", 1)'Img);  --  should be 24
   end;

   ColdFrame.Test_Stub_Support.Tear_Down;

end ColdFrame.Test_Stub_Support_Test;

