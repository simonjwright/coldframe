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

--  $RCSfile: normalize_xmi-identifiers.ads,v $
--  $Revision: 12a6c3b1d22b $
--  $Date: 2012/01/22 19:05:53 $
--  $Author: simonjwright $

private package Normalize_XMI.Identifiers is

   procedure Read_Case_Exceptions (From : String);
   --  Read case exceptions, in Emacs Ada-mode form, from files
   --  'From'.  'From' may be a list of files, separated by ':' on
   --  Unix, ';' on Windows.

   function Normalize (Id : String) return String;

   --  Form an abbreviation for the (Class) name.
   --
   --  If the name consists of more than one word, make one up from
   --  the initial letters of Name (which will already have been
   --  capitalised)
   --
   --  Otherwise, prefix Name with A_ or An_.
   function Abbreviate (Name : String) return String;

   --  The function Find_Spans is a utility to help in processing
   --  strings containing multiple components separated by a single
   --  separator character; for example a qualified name, Ada.Text_IO,
   --  split at '.', would contain two spans:
   --
   --  Ada.Text_IO
   --  L-U L-----U
   --
   --  Consecutive occurrences of the separator character result in an
   --  empty Span (U < L).
   type Span is record
      L : Natural;
      U : Natural := 0;
   end record;
   type Spans is array (Natural range <>) of Span;

   function Find_Spans (S : String; Splitting_At : Character) return Spans;

end Normalize_XMI.Identifiers;
