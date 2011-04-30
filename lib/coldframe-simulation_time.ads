--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Real_Time;

package ColdFrame.Simulation_Time is

   Use_Error : exception;

   type Time is private;

   function Zero return Time;
   --  Returns the Time at which Start was called.
   --  Raises Use_Error if not Started.

   function Clock return Time;
   --  Returns the current Time.
   --  Raises Use_Error if not Started.

   function "+"  (Left : Time; Right : Duration) return Time;
   function "+"  (Left : Duration; Right : Time) return Time;
   function "-"  (Left : Time; Right : Duration) return Time;
   function "-"  (Left : Time; Right : Time) return Duration;

   function "<"  (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">"  (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   function Image (T : Time) return String;

   function To_Real_Time (T : Time) return Ada.Real_Time.Time;
   --  Raises Use_Error if not Started.

   procedure Start;
   --  Start the simulation clock. There's no stopping it.

private

   type Time is new Duration;

end ColdFrame.Simulation_Time;
