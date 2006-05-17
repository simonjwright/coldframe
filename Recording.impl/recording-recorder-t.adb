--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  Part of the Recording demonstration.

--  $RCSfile: recording-recorder-t.adb,v $
--  $Revision: 3f1d0e8217cf $
--  $Date: 2006/05/17 05:58:12 $
--  $Author: simonjwright $

with Ada.Dynamic_Priorities;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with System;

separate (Recording.Recorder)
task body T is
begin

   accept Start;

   --  Run at a lower priority than the main application. The amount
   --  by which to run lower would be adaptable.
   Ada.Dynamic_Priorities.Set_Priority (System.Default_Priority - 1);

   loop
      declare
         Str : Stream;
      begin

         This.Buff.Fetch_Stream (Str);

         begin
            --  output the contents of Str to This.Socket
            delay 0.1;   --  simulation!
         exception
            when E : others =>
               Put_Line (Ada.Exceptions.Exception_Information (E));
               delay 1.0;  -- avoid tight loop
         end;

         This.Buff.Done;

      end;
   end loop;

end T;
