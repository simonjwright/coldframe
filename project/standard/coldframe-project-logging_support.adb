--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project logging.
--
--  This is ColdFrame's default implementation.

with Ada.Text_IO;
with ColdFrame.Synchronization;

package body ColdFrame.Project.Logging_Support is


   --  Used to protect access to the "output log message" "resource".
   Sem : aliased ColdFrame.Synchronization.Semaphore;


   procedure Log (Severity : Severity_Code; Message : String) is
      --  Lock the resource ..
      L : ColdFrame.Synchronization.Lock (Sem'Access);
      pragma Warnings (Off, L);
      --  .. resource now locked.
   begin
      Ada.Text_IO.Put_Line ("ColdFrame: " & Severity'Img & ": " & Message);
      Ada.Text_IO.Flush;
      --  to be sure not to lose anything in case of later exception
      --  (particularly in the debugger).
   end Log;


end ColdFrame.Project.Logging_Support;
