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
--  $Revision: 281d11e491da $
--  $Date: 2002/07/27 13:05:23 $
--  $Author: simon $

package ColdFrame.Project is

   --  ColdFrame requires certain project-specific instantiations of
   --  support software to be made available in a package hierarchy
   --  rooted at this package.
   --
   --  Specifically,
   --
   --  ColdFrame.Project.Calendar provides the type Time used to
   --  translate attributes and parameters of type Date or Time
   --
   --  ColdFrame.Project.Events must contain an instantiation of
   --  ColdFrame.Events_G
   --
   --  ColdFrame.Project.Global_Storage_Pool must contain an object
   --  Pool of type System.Storage_Pools.Root_Storage_Pool'Class

end ColdFrame.Project;
