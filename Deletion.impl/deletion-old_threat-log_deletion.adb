with Ada.Text_IO;

separate (Deletion.Old_Threat)
procedure Log_Deletion
  (This : Handle) is
begin
   Ada.Text_IO.Put_Line
     ("Deletion.Old_Threat deleted.");
end Log_Deletion;
