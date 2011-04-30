with Ada.Text_IO; use Ada.Text_IO;
separate (Regressions.Event_Holder)
task body T is
begin
   delay 5.0;
   Put_Line (Standard_Error, "Event_Holder.T passed its delay");
end T;
