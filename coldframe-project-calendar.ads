--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to support time
--  management.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-calendar.ads,v $
--  $Revision: c02acd3df96c $
--  $Date: 2003/01/22 19:51:15 $
--  $Author: simon $

with Ada.Calendar;

package ColdFrame.Project.Calendar is

   subtype Time is Ada.Calendar.Time;

   function Clock return Time renames Ada.Calendar.Clock;

   function Image (T : Time) return String;
   --  Returns a string version of Time for use in generated
   --  Serialization.Image functions.
   --  This implementation returns time as a Duration since 1 Jan 1970 00:00.

end ColdFrame.Project.Calendar;
