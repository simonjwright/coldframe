--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project logging.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-logging_support.adb,v $
--  $Revision: e91d25f64219 $
--  $Date: 2002/10/01 17:43:10 $
--  $Author: simon $

with GNAT.IO;

package body ColdFrame.Project.Logging_Support is


   procedure Log (Severity : Severity_Code; Message : String) is
   begin
      GNAT.IO.Put_Line ("ColdFrame: " & Severity'Img & ": " & Message);
   end Log;


end ColdFrame.Project.Logging_Support;
