--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to support time
--  management.
--
--  This is ColdFrame's default implementation.

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Calendar;

package ColdFrame.Project.Calendar is

   subtype Time is Ada.Calendar.Time;

   function Clock return Time renames Ada.Calendar.Clock;

   function Epoch return Time;
   --  Returns a value suitable for use in aggregates.

   function Image (T : Time) return String;
   --  Returns a string version of Time for use in generated
   --  Serialization.Image functions.
   --  This implementation returns time as a Duration since 1 Jan 1970 00:00.

end ColdFrame.Project.Calendar;
