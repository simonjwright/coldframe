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

--  $RCSfile: normalize_xmi-model-generalizations.ads,v $
--  $Revision: 4832d3f648a3 $
--  $Date: 2012/01/25 15:17:08 $
--  $Author: simonjwright $

private package Normalize_XMI.Model.Generalizations is

   type Generalization_Element is new Element with private;

   --  Read_Generalization is called with From designating a single
   --  {name, parent, child} tuple.
   --
   --  It accumulates the results in Accumulating_In, which is a map
   --  of Generalization_Elements.
   procedure Read_Generalization
     (From            :        not null DOM.Core.Node;
      Parent          :        not null Element_P;
      Accumulating_In : in out Element_Maps.Map);

private

   type Generalization_Element is new Element with record
      Parent_Class : Element_P;
      Child_Classes : Element_Vectors.Vector;
   end record;

   overriding
   procedure Resolve (G : in out Generalization_Element);
   overriding
   procedure Output (G : Generalization_Element; To : Ada.Text_IO.File_Type);

end Normalize_XMI.Model.Generalizations;
