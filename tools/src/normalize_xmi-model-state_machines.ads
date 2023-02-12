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

private package Normalize_XMI.Model.State_Machines is

   function Read_State_Machine (From   : not null DOM.Core.Node;
                                Parent : not null Element_P) return Element_P;

private

   --  The elements that go to make up a State Machine are all in the
   --  same unit, because they need mutual visibility and aren't
   --  otherwise shared.

   type State_Machine_Element is new Element with record
      Events      : Element_Maps.Map;        --  Keyed by "xmi.id"
      States      : Element_Maps.Map;        --  Keyed by "xmi.id"
      Transitions : Element_Vectors.Vector;  --  No useful key, not referenced
   end record;
   overriding
   procedure Resolve (S : in out State_Machine_Element);
   overriding
   procedure Output (S : State_Machine_Element; To : Ada.Text_IO.File_Type);

end Normalize_XMI.Model.State_Machines;
