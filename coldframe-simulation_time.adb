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

--  $RCSfile: coldframe-simulation_time.adb,v $
--  $Revision: f3012f882b87 $
--  $Date: 2002/07/04 04:55:48 $
--  $Author: simon $

package body ColdFrame.Simulation_Time is

   Running : Boolean := False;

   Start_Time : Ada.Real_Time.Time;


   function Clock return Time is
      use type Ada.Real_Time.Time;
   begin
      if not Running then
         raise Use_Error;
      end if;
      return Time
        (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Start_Time));
   end Clock;


   function "+" (Left : Time; Right : Duration) return Time is
   begin
      return Time (Duration (Left) + Right);
   end "+";


   function "+"  (Left : Duration; Right : Time) return Time is
   begin
      return Time (Left + Duration (Right));
   end "+";


   function "-"  (Left : Time; Right : Duration) return Time is
   begin
      return Time (Duration (Left) - Right);
   end "-";


   function "-"  (Left : Time; Right : Time) return Duration is
   begin
      return Duration (Left) - Duration (Right);
   end "-";


   function "<"  (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) < Duration (Right);
   end "<";


   function "<=" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) <= Duration (Right);
   end "<=";


   function ">"  (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) > Duration (Right);
   end ">";


   function ">=" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) >= Duration (Right);
   end ">=";


   function Image (T : Time) return String is
   begin
      return Duration'Image (Duration (T));
   end Image;


   function To_Real_Time (T : Time) return Ada.Real_Time.Time is
      use type Ada.Real_Time.Time_Span;
   begin
      if not Running then
         raise Use_Error;
      end if;
      return Start_Time + Ada.Real_Time.To_Time_Span (Duration (T));
   end To_Real_Time;


   procedure Start is
   begin
      if Running then
         raise Use_Error;
      end if;
      Start_Time := Ada.Real_Time.Clock;
      Running := True;
   end Start;


end ColdFrame.Simulation_Time;
