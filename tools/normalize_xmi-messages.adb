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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Text_IO; use Ada.Text_IO;

package body Normalize_XMI.Messages is


   Warnings : Natural := 0;
   Errors : Natural := 0;


   procedure Warning (Message : String)
   is
   begin
      Warnings := Warnings + 1;
      Put_Line (Standard_Error, "Warning: " & Message);
   end Warning;

   function Number_Of_Warnings return Natural
   is
   begin
      return Warnings;
   end Number_Of_Warnings;


   procedure Error (Message : String)
   is
   begin
      Errors := Errors + 1;
      Put_Line (Standard_Error, "Error: " & Message);
   end Error;


   function Number_Of_Errors return Natural
   is
   begin
      return Errors;
   end Number_Of_Errors;


end Normalize_XMI.Messages;
