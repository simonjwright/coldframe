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

--  This unit is to be used with EWS to help generate web pages
--  showing current event statistics.

--  $RCSfile: coldframe-logging_event_basis-ews_support.ads,v $
--  $Revision: 78558c644edb $
--  $Date: 2003/11/16 08:22:38 $
--  $Author: simon $

with EWS.Dynamic;
with EWS.HTTP;

package ColdFrame.Logging_Event_Basis.EWS_Support is

   procedure Add_Section
     (For_Request : EWS.HTTP.Request_P;
      To : in out EWS.Dynamic.Dynamic_Response);

   function Whole_Page
     (From_Request : EWS.HTTP.Request_P)
     return EWS.Dynamic.Dynamic_Response'Class;

end ColdFrame.Logging_Event_Basis.EWS_Support;
