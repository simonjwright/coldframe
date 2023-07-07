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

--  Derived from Terry Westley's TWAShell (Tcl Windowing Ada SHell).

with CArgv;
with GNAT.Exception_Traces;
with Interfaces.C;
with Tcl.Tk;

with Stairwell_Demo_Application;

procedure Stairwell_Demo is

   --  Argc and Argv include the command name
   Argc : Interfaces.C.int;
   Argv : CArgv.Chars_Ptr_Ptr;

begin

   GNAT.Exception_Traces.Trace_On
     (Kind => GNAT.Exception_Traces.Unhandled_Raise);

   --  Get command-line arguments and put them into C-style "argv",
   --  as required by Tk_Main.
   CArgv.Create (Argc, Argv);

   --  Start Tcl
   Tcl.Tk.Tk_Main (Argc, Argv, Stairwell_Demo_Application.Init'Access);

end Stairwell_Demo;
