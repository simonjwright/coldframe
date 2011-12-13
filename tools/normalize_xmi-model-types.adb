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

--  $RCSfile: normalize_xmi-model-types.adb,v $
--  $Revision: 2e42ac7f6e38 $
--  $Date: 2011/12/13 17:12:41 $
--  $Author: simonjwright $

with Normalize_XMI.Errors;

package body Normalize_XMI.Model.Types is


   function Read_Type (From : DOM.Core.Node) return Element_P
   is
      use Ada.Text_IO;
      N : constant Element_P := new Type_Element;
      T : Type_Element renames Type_Element (N.all);
   begin
      T.Populate (From => From);
      T.Name := +Read_Name (From_Element => From);
      Put_Line (Standard_Error, "... reading type " & (+T.Name));
      return N;
   end Read_Type;


   overriding
   procedure Resolve (T : in out Type_Element)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "... checking type " & (+T.Name));
      if T.Has_Tag ("imported") and T.Has_Tag ("renames") then
         Errors.Report
           ("Type "
              & (+T.Name)
              & " has both {imported} and {renames} specified.");
      end if;
   end Resolve;


   overriding
   procedure Output (T : Type_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<type");
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+T.Name) & "</name>");
      T.Output_Documentation (To);
      if T.Has_Tag ("imported") then
         Put_Line (To,
                   "<imported>" & T.Tag_As_Name ("imported") & "</imported>");
      end if;
      if T.Has_Tag ("renames") then
         Put_Line (To, "<renames>" & T.Tag_As_Name ("renames") & "</renames>");
      end if;
      Put_Line (To, "</type>");
   end Output;


end Normalize_XMI.Model.Types;
