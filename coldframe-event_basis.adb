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

--  $RCSfile: coldframe-event_basis.adb,v $
--  $Revision: 34985551f126 $
--  $Date: 2003/11/08 08:14:22 $
--  $Author: simon $

package body ColdFrame.Event_Basis is


   procedure Log (The_Event : access Event_Base;
                  At_Phase : Event_Processing_Phase) is
      pragma Warnings (Off, The_Event);
      pragma Warnings (Off, At_Phase);
   begin
      null;
   end Log;


end ColdFrame.Event_Basis;