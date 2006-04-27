--  Buffers a data value for recording.

separate (Recording.Recorder)
procedure Take_Record
  (Item : Recordable) is
   Str : Stream;
begin
   This.Buff.Get_Stream (Str);
   if Str /= null then
      Recordable'Output (Str, Item);
   end if;
end Take_Record;
