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

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile: coldframe-serialization_signature.ads,v $
--  $Revision: 986d85db4d3e $
--  $Date: 2003/02/02 19:07:46 $
--  $Author: simon $

--  This package specifies the characteristics required to support the
--  translation of {serialization}.
--
--  ColdFrame.Project.Serialization is expected to provide the
--  facilities required by this package, so it will be easiest to
--  provide is as an instantiation of this package.

generic

   type Actual_Base is abstract tagged private;
   --  The basis for all serialized values. It should have a primitive
   --  function
   --
   --     function Image (S : Actual_Base) return String;
   --
   --  since such a function is generated in Domain.Serialization.

   with function Actual_Class_Image (B : Actual_Base'Class) return String;
   --  This function must dispatch to the generated Image function (there
   --  is no way to invoke the primitive Image directly).
   --
   --  A satisfactory implementation is
   --
   --     function Class_Image (S : Actual_Base'Class) return String is
   --     begin
   --        return Image (S);
   --     end Class_Image;

package ColdFrame.Serialization_Signature is

   subtype Base is Actual_Base;

   function Class_Image (B : Base'Class) return String
     renames Actual_Class_Image;

end ColdFrame.Serialization_Signature;
