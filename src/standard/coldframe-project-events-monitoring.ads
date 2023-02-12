--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports monitored project events.
--
--  This is ColdFrame's default implementation.

with ColdFrame.Events_G.Monitoring_G;
with ColdFrame.Project.Held_Events.Signature;
with ColdFrame.Project.High_Resolution_Time;

package ColdFrame.Project.Events.Monitoring
is new Project.Events.Monitoring_G
  (Held_Events => Held_Events.Signature,
   High_Resolution_Time => ColdFrame.Project.High_Resolution_Time.Time,
   Clock => ColdFrame.Project.High_Resolution_Time.Clock,
   "-" => ColdFrame.Project.High_Resolution_Time."-");
