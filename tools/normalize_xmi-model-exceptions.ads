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

--  $RCSfile: normalize_xmi-model-exceptions.ads,v $
--  $Revision: fd881b1b7e39 $
--  $Date: 2011/12/11 15:20:54 $
--  $Author: simonjwright $

private package Normalize_XMI.Model.Exceptions is

   function Read_Exception (From : DOM.Core.Node) return Element_P;

private

   type Exception_Element is new Element with record
      Renaming : Ada.Strings.Unbounded.Unbounded_String;
      Imported : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   procedure Resolve (E : in out Exception_Element);
   procedure Output (E : Exception_Element; To : Ada.Text_IO.File_Type);

end Normalize_XMI.Model.Exceptions;
