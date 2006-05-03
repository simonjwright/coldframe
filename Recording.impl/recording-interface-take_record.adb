--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  Part of the Recording demonstration.

--  $RCSfile: recording-interface-take_record.adb,v $
--  $Revision: ef76ca9d66ef $
--  $Date: 2006/05/03 22:07:21 $
--  $Author: simonjwright $

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
