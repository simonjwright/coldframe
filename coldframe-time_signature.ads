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

--  $RCSfile: coldframe-time_signature.ads,v $
--  $Revision: 92e3bf5831c2 $
--  $Date: 2004/10/29 05:04:37 $
--  $Author: simon $

generic

   --  This package specifies the properties required of a Time for it
   --  to be used with event management.

   type Time_Kind is (<>);

   --  One of the kinds of Time must be Real_Time, used for "delay for".
   Real_Time : Time_Kind;

   type Time (Kind : Time_Kind) is private;

   with function From_Now (Period : Duration) return Time;

   with function Image (Of_Time : Time) return String;
   --  Used in logging versions of Event Queues.

package ColdFrame.Time_Signature is
private
   --  Turn off GNAT's warnings (5.02a1)
   pragma Warnings (Off, Real_Time);
   pragma Warnings (Off, From_Now);
   pragma Warnings (Off, Image);
end ColdFrame.Time_Signature;
