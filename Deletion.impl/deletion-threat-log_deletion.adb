with Ada.Text_IO;

separate (Deletion.Threat)
procedure Log_Deletion
  (This : Handle) is
begin
   Ada.Text_IO.Put_Line
     ("Deletion.Threat" & Integer'Image (This.Id) & " deleted.");
end Log_Deletion;
