with Ada.Text_IO;

separate (Deletion.New_Threat)
procedure Log_Deletion
  (This : Handle) is
begin
   Ada.Text_IO.Put_Line
     ("Deletion.New_Threat deleted.");
end Log_Deletion;
