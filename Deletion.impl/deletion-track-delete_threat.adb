separate (Deletion.Track)
procedure Delete_Threat
  (This : Handle) is
   H : Handle := This;
begin
   Delete_Track_And_Threat (This);
   Delete (H);
end Delete_Threat;
