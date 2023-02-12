--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to support time
--  management.
--
--  This is ColdFrame's default implementation for Ravenscar.

package body ColdFrame.Project.Calendar is

   function "-" (L, R : Time) return Duration
   is
   begin
      return Ada.Real_Time.To_Duration (Ada.Real_Time."-" (L, R));
   end "-";

   function Epoch return Time is
   begin
      return Ada.Real_Time.Time_First;
   end Epoch;

end ColdFrame.Project.Calendar;
