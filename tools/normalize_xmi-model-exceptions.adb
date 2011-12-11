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
--  $Revision: fd881b1b7e39 $
--  $Date: 2011/12/11 15:20:54 $
--  $Author: simonjwright $

with Normalize_XMI.Errors;

package body Normalize_XMI.Model.Exceptions is


   function Read_Exception (From : DOM.Core.Node) return Element_P
   is
      N : constant Element_P := new Exception_Element;
      E : Exception_Element renames Exception_Element (N.all);
   begin
      E.Name := +Read_Name (From_Element => From);
      E.Documentation
        := +Read_Tagged_Value ("documentation", From_Element => From);
      E.Renaming
        := +Read_Tagged_Name ("renames", From_Element => From);
      E.Imported
        := +Read_Tagged_Name ("imported", From_Element => From);
      return N;
   end Read_Exception;


   procedure Resolve (E : in out Exception_Element)
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "... checking exception " & (+E.Name));
      if Length (E.Imported) > 0 and Length (E.Renaming) > 0 then
         Errors.Report
           ("Exception "
              & (+E.Name)
              & " has both <<imported>> and <<renames>> specified.");
      end if;
   end Resolve;


   procedure Output (E : Exception_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
   begin
      Put (To, "<exception");
      if Length (E.Renaming) > 0 then
         Put (To, " renames=""" & (+E.Renaming) & """");
      end if;
      if Length (E.Imported) > 0 then
         Put (To, " imported=""" & (+E.Imported) & """");
      end if;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+E.Name) & "</name>");
      Output_Documentation (+E.Documentation, To);
      Put_Line (To, "</exception>");
   end Output;


end Normalize_XMI.Model.Exceptions;
