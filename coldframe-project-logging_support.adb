--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project logging.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-logging_support.adb,v $
--  $Revision: cbfc1e36c26c $
--  $Date: 2003/10/09 18:41:13 $
--  $Author: simon $

with Ada.Text_IO;
with BC.Support.Synchronization;

package body ColdFrame.Project.Logging_Support is


   --  Used to protect access to the "output log message" "resource".
   Sem : aliased BC.Support.Synchronization.Semaphore;


   procedure Log (Severity : Severity_Code; Message : String) is
      --  Lock the resource ..
      L : BC.Support.Synchronization.Lock (Sem'Access);
      pragma Warnings (Off, L);
      --  .. resource now locked.
   begin
      Ada.Text_IO.Put_Line ("ColdFrame: " & Severity'Img & ": " & Message);
      Ada.Text_IO.Flush;
      --  to be sure not to lose anything in case of later exception
      --  (particularly in the debugger).
   end Log;


end ColdFrame.Project.Logging_Support;
