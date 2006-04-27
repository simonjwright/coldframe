with Recording.Recorder;

--  Buffers a data value for recording.

--  If the recording buffer is full, the data will be silently dropped
--  (though statistics on the number of dropped records may be kept
--  and output).

separate (Recording.Interface)
procedure Take_Record
  (Item : Recordable) is
   pragma Assert (Domain_Initialized, "Recording not initialized");
begin
   Recorder.Take_Record (Item);
end Take_Record;
