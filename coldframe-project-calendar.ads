--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to support time
--  management.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-calendar.ads,v $
--  $Revision: caa42b593e4d $
--  $Date: 2002/08/20 18:33:23 $
--  $Author: simon $

with Ada.Calendar;

package ColdFrame.Project.Calendar is

   subtype Time is Ada.Calendar.Time;

   function Clock return Time renames Ada.Calendar.Clock;

end ColdFrame.Project.Calendar;
