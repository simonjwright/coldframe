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

--  $RCSfile: normalize_xmi-model-exceptions.adb,v $
--  $Revision: 093f39d61362 $
--  $Date: 2011/12/14 21:26:48 $
--  $Author: simonjwright $

with Normalize_XMI.Messages;

package body Normalize_XMI.Model.Exceptions is


   function Read_Exception (From : DOM.Core.Node) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Exception_Element;
      E : Exception_Element renames Exception_Element (N.all);
   begin
      E.Populate (From => From);
      E.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "... reading exception " & (+E.Name));
      return N;
   end Read_Exception;


   overriding
   procedure Resolve (E : in out Exception_Element)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "... checking exception " & (+E.Name));
      if E.Has_Tag ("imported") and E.Has_Tag ("renames") then
         Messages.Error
           ("Exception "
              & (+E.Name)
              & " has both {imported} and {renames} specified.");
      end if;
   end Resolve;


   overriding
   procedure Output (E : Exception_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<exception");
      if E.Has_Tag ("imported") then
         Put (To, " imported=""" & E.Tag_As_Name ("imported") & """");
      end if;
      if E.Has_Tag ("renames") then
         Put (To, " renames=""" & E.Tag_As_Name ("renames") & """");
      end if;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+E.Name) & "</name>");
      E.Output_Documentation (To);
      Put_Line (To, "</exception>");
   end Output;


end Normalize_XMI.Model.Exceptions;
