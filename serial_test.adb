with Serial_Test_Support;
with GNAT.IO; use GNAT.IO;

procedure Serial_Test is

   V : Serial_Test_Support.T
     := (Serial_Test_Support.Base with I => 42, F => 73.0, C => 'E');

begin

   Put_Line (Serial_Test_Support.Image (V));

end Serial_Test;
