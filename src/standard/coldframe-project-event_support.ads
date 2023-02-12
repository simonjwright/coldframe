--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

with ColdFrame.Time_Signature;
with ColdFrame.Project.Times;

package ColdFrame.Project.Event_Support is

   package Signature is new ColdFrame.Time_Signature
     (Time_Kind => Times.Time_Kind,
      Real_Time => Times.Real_Time,
      Time => Times.Time,
      From_Now => Times.From_Now,
      Image => Times.Image);

end ColdFrame.Project.Event_Support;
