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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with BC.Containers.Maps.Bounded;
with BC.Containers.Maps.Unbounded;
with ColdFrame.Project.Storage_Pools;

package ColdFrame.Instances is


   --  NOTE, none  of the operations in this  package are intended  --
   --  to be called directly by the user.                           --


   type Instance_Base is abstract tagged limited private;
   --  All Instances are derived from this type.
   --
   --  The purpose is to allow mutual visibility (to support
   --  associations and inheritance relationships) without using
   --  non-standard extensions such as WITH TYPE.

   function Instance_Identifier_Equality (L, R : Instance_Base) return Boolean;
   --  Returns True if the Identifiers of L, R are equal.
   --  The default raises Program_Error (it should only be called in
   --  the context of a Map, and that should only happen for classes
   --  with identifiers).

   function Instance_Hash (Of_The_Instance : Instance_Base) return Natural;
   --  Generates a hash from the Identifier of Of_The_Instance.
   --  The default returns 0.
   --
   --  It's called Instance_Hash (probably temporarily) to avoid
   --  confusion with the old Hash, which had a different profile.

   type Handle is access all Instance_Base'Class;
   for Handle'Storage_Size use 0;

   function Classwide_Identifier_Equality (L, R : Handle) return Boolean;
   --  Dispatches to the appropriate Instance_Identifier_Equality.

   function Classwide_Hash (Of_The_Handle : Handle) return Natural;
   --  Dispatches to the appropriate Instance_Hash.

   --  Map instantiations.
   package Abstract_Containers is new BC.Containers (Handle);
   package Abstract_Maps is new Abstract_Containers.Maps
     (Key => Handle,
      "=" => Classwide_Identifier_Equality);

   package Bounded_Maps is new Abstract_Maps.Bounded
     (Hash => Classwide_Hash,
      Buckets => 19,               --  the default
      Maximum_Size => 19);         --  the default

   package Unbounded_Maps is new Abstract_Maps.Unbounded
     (Hash => Classwide_Hash,
      Buckets => 19,               --  the default
      Storage => ColdFrame.Project.Storage_Pools.Pool);

private

   type Instance_Base is abstract tagged limited null record;

end ColdFrame.Instances;
