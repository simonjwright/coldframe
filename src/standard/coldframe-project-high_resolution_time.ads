--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame's event queue performance logging, in
--  ColdFrame.Logging_Event_Basis, expects this package to exist to
--  support high resolution (sub-microsecond) timing. This version
--  just uses Ada.Calendar.Time, which is fine on Linux and Mac OS X.

with Ada.Calendar;
package ColdFrame.Project.High_Resolution_Time
  renames Ada.Calendar;
