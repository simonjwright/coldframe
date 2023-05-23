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

--  The standard ColdFrame time support involves a private type Time
--  in ColdFrame.Project.Times which is intended to support various
--  different time frames (e.g. Real_Time, Calendar,
--  project-specific). This unit is for the Ravenscar version, which
--  only handles Real_Time; ColdFrame.Project.Times is a renaming of
--  this unit, and allows code written for the desktop ColdFrame
--  environment to compile unchanged in the restricted Ravenscar
--  environment.

with Ada.Real_Time;

package ColdFrame.Times is

   subtype Time is Ada.Real_Time.Time;

   --  Creation operations

   function Create (From_Time : Ada.Real_Time.Time) return Time
     is (From_Time);

   --  Operations to support Time_Signature

   function From_Now (Period : Duration) return Time
     is (Ada.Real_Time."+" (Ada.Real_Time.Clock,
                            Ada.Real_Time.To_Time_Span (Period)));

   pragma Warnings (Off, "formal parameter*is not referenced");
   function Image (Of_Time : Time) return String
     is ("");
   pragma Warnings (On, "formal parameter*is not referenced");

   --  Additional operations

   function Equivalent (Of_Time : Time) return Ada.Real_Time.Time
     is (Of_Time);

   function "<" (L, R : Time) return Boolean
     renames Ada.Real_Time."<";

end ColdFrame.Times;
