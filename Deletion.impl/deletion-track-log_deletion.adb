with Ada.Text_IO;

separate (Deletion.Track)
procedure Log_Deletion
  (This : Handle) is
begin
   Ada.Text_IO.Put_Line
     ("Deletion.Track" & Integer'Image (This.Id) & " deleted.");
end Log_Deletion;
