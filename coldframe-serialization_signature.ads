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
--  $Revision: b5a1f4ce42f2 $
--  $Date: 2003/01/26 16:35:10 $
--  $Author: simon $

--  This package specifies the characteristics required to support the
--  translation of {serialization}.
--
--  ColdFrame.Project.Serialization is expected to provide the
--  facilities required by this package, so it will be easiest to
--  provide is as an instantiation of this package.

generic

   type Actual_Base is abstract tagged private;

   with function Actual_Image (B : Actual_Base'Class) return String;

package ColdFrame.Serialization_Signature is

   subtype Base is Actual_Base;

   function Image (B : Base'Class) return String renames Actual_Image;

end ColdFrame.Serialization_Signature;
