--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  Regression tests for ColdFrame.
--
--  The units with'ed by the body only have to compile.

with Compilation_Regressions.Has_Event_Named_Next.All_Instances;
with Compilation_Regressions.Has_Event_Named_Next.Selection_Function;
with Compilation_Regressions.Has_Event_Named_Next.CF_Tear_Down;
separate (Compilation_Regressions.Has_Event_Named_Next)
procedure Caller
  (This : Handle) is
   Unimplemented : exception;
begin
   raise Unimplemented;
end Caller;
