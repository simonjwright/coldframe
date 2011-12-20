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

--  $RCSfile: normalize_xmi-model-association_ends.ads,v $
--  $Revision: 113b7da65bbd $
--  $Date: 2011/12/20 21:01:07 $
--  $Author: simonjwright $

private package Normalize_XMI.Model.Association_Ends is

   function Read_Association_End
     (From : DOM.Core.Node;
      Parent : not null Element_P) return Element_P;

   --  The details aren't private, because the Association will need to
   --  mess with them (it has to see both Association_Ends).

   type Bound is (Zero, One, Many);
   type Lower_Bound is new Bound range Zero .. One;
   type Upper_Bound is new Bound range One .. Many;

   type Association_End_Element is new Element with record
      Participant : Ada.Strings.Unbounded.Unbounded_String;
      Lower : Lower_Bound;
      Upper : Upper_Bound;
      Source : Boolean; -- Initially from <<source>>.
   end record;
   overriding
   procedure Resolve (E : in out Association_End_Element);
   overriding
   procedure Output (E : Association_End_Element; To : Ada.Text_IO.File_Type);

end Normalize_XMI.Model.Association_Ends;
