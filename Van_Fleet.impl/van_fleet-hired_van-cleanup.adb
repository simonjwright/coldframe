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

--  $RCSfile: van_fleet-hired_van-cleanup.adb,v $
--  $Revision: af9296feed72 $
--  $Date: 2004/05/08 18:58:13 $
--  $Author: simon $

with Van_Fleet.A2;

separate (Van_Fleet.Hired_Van)
procedure Cleanup
  (This : Handle) is
begin
   A2.Unlink
     (Is_Borrowing => This,
      Is_On_Loan_To => A2.Is_Borrowing (This));
end Cleanup;
