with Ada.Text_IO; use Ada.Text_IO;
separate (Regressions.Find_Active_Singleton)
task body T is
begin
   delay 5.0;
   Put_Line (Standard_Error, "Find_Active_Singleton.T passed its delay");
end T;
