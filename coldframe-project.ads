--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile: coldframe-project.ads,v $
--  $Revision: e9da3cc4beaf $
--  $Date: 2004/05/11 05:14:49 $
--  $Author: simon $

package ColdFrame.Project is

   --  ColdFrame requires certain project-specific instantiations of
   --  support software to be made available in a package hierarchy
   --  rooted at this package.
   --
   --  Specifically,
   --
   --  ColdFrame.Project.Calendar provides the type Time used to
   --  translate attributes and parameters of type Date or Time, the
   --  function Clock used to generate default return values, and the
   --  function Image used for (de)serialization
   --
   --  ColdFrame.Project.Events must contain an instantiation of
   --  ColdFrame.Events_G
   --
   --  ColdFrame.Project.Log_Error must be an error-logging procedure
   --
   --  ColdFrame.Project.Serialization must provide an abstract tagged
   --  type Base and operations Image, Base_Attribute_Image as in
   --  ColdFrame.Serialization
   --
   --  ColdFrame.Project.Storage_Pools must contain
   --  * an object Pool, used for unbounded allocations, of type
   --    System.Storage_Pools.Root_Storage_Pool'Class
   --  * a (sub)type Bounded_Pool, used for bounded allocations, like
   --      type Bounded_Pool
   --        (Pool_Size : System.Storage_Elements.Storage_Count;
   --         Elmt_Size : System.Storage_Elements.Storage_Count;
   --         Alignment : System.Storage_Elements.Storage_Count)
   --      is new System.Storage_Pools.Root_Storage_Pool with private;

end ColdFrame.Project;
