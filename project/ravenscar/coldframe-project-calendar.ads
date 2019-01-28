--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to support time
--  management.
--
--  This is ColdFrame's default implementation for Ravenscar.

with Ada.Real_Time;

package ColdFrame.Project.Calendar is

   subtype Time is Ada.Real_Time.Time;

   function "-" (L, R : Time) return Duration;

   function Clock return Time renames Ada.Real_Time.Clock;

   function Epoch return Time;
   --  Returns a value suitable for use in aggregates.

end ColdFrame.Project.Calendar;
