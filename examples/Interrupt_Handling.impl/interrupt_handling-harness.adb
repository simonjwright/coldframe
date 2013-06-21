with Ada.Text_IO; use Ada.Text_IO;
with Interrupt_Handling.Initialize;

procedure Interrupt_Handling.Harness is
begin
   New_Line;
   Put_Line ("Interrupt_Handling.Harness.");
   Put_Line ("---------------------------");
   New_Line;
   Put_Line ("This program demonstrates handling a Signal, which is the");
   Put_Line ("nearest thing to an interrupt available in standard OSs.");
   New_Line;
   Put_Line ("On Mac OS X and Linux systems, run the program and type C-c");
   Put_Line ("(ctrl-c) to see the interrupt being reported.");
   New_Line;
   Put_Line ("On Windows, this doesn't work from a Cygwin Bash shell;");
   Put_Line ("instead, run from a Command Shell started from Start/Run.");
   New_Line;
   Flush;
   delay 1.0;
   Initialize;
end Interrupt_Handling.Harness;
