with Ada.Text_IO;

separate (Deletion.Command_Threat)
procedure Log_Deletion
  (This : Handle) is
begin
   Ada.Text_IO.Put_Line
     ("Deletion.Command_Threat deleted.");
end Log_Deletion;
