with Ada.Text_IO;

separate (Deletion.Not_Threat)
procedure Log_Deletion
  (This : Handle) is
begin
   Ada.Text_IO.Put_Line
     ("Deletion.Not_Threat deleted.");
end Log_Deletion;
