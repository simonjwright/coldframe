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

--  $RCSfile: coldframe-instances.ads,v $
--  $Revision: 26afcc6719ee $
--  $Date: 2003/07/12 16:23:32 $
--  $Author: simon $

with BC.Containers.Maps.Bounded;
with BC.Containers.Maps.Unbounded;
with ColdFrame.Project.Global_Storage_Pool;

package ColdFrame.Instances is

   type Instance_Base is abstract tagged limited private;
   --  All Instances are derived from this type.
   --
   --  The purpose is to allow mutual visibility (to support
   --  associations and inheritance relationships) without using
   --  non-standard extensions such as WITH TYPE.

   function Instance_Hash (Of_The_Instance : Instance_Base) return Natural;
   --  Generates a hash from the Identifier of Of_The_Instance.
   --  The default returns 0.

   type Handle is access all Instance_Base'Class;
   for Handle'Storage_Size use 0;

   function Classwide_Hash (Of_The_Handle : Handle) return Natural;
   --  Dispatches to the appropriate Instance_Hash.

   package Abstract_Containers is new BC.Containers (Handle);
   package Abstract_Maps is new Abstract_Containers.Maps (Handle);

   package Bounded_Maps is new Abstract_Maps.Bounded
     (Hash => Classwide_Hash,
      Buckets => 19,               --  the default
      Maximum_Size => 19);         --  the default

   package Unbounded_Maps is new Abstract_Maps.Unbounded
     (Hash => Classwide_Hash,
      Buckets => 19,               --  the default
      Storage => ColdFrame.Project.Global_Storage_Pool.Pool);

private

   type Instance_Base is abstract tagged limited null record;

end ColdFrame.Instances;
