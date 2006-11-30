--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  Part of the Recording demonstration.

--  $RCSfile: recording-recorder-take_record.adb,v $
--  $Revision: ef76ca9d66ef $
--  $Date: 2006/05/03 22:07:21 $
--  $Author: simonjwright $

--  Buffers a data value for recording.

separate (Recording.Recorder)
procedure Take_Record
  (Item : Recordable) is
   Str : Stream;
   Capacity : constant Positive := 256;  --  would be adaptable
begin
   This.Buff.Get_Stream (Str, Capacity => Capacity);
   if Str /= null then
      Recordable'Output (Str, Item);
   end if;
end Take_Record;