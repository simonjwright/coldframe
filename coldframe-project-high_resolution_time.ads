--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame's event queue performance logging, in
--  ColdFrame.Logging_Event_Basis, expects this package to exist to
--  support high resolution (sub-microsecond) timing.

--  $RCSfile: coldframe-project-high_resolution_time.ads,v $
--  $Revision: 5fd905d8f2c5 $
--  $Date: 2003/11/15 08:24:49 $
--  $Author: simon $

with BC.Support.High_Resolution_Time;
package ColdFrame.Project.High_Resolution_Time
  renames BC.Support.High_Resolution_Time;
