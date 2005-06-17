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
--  $Revision: a47004786a59 $
--  $Date: 2005/06/17 05:20:20 $
--  $Author: simonjwright $

--  This package specifies the support required for {serializable}
--  <<type>> classes.
--
--  The package ColdFrame.Project.Serialization should be an
--  instantiation of this package.
--
--  Alternatively, it could be a re-implementation of that package
--  with the addition of the Deserialize constant.

with ColdFrame.Serialization;

generic

   type Actual_Base is abstract new ColdFrame.Serialization.Base with private;

   Deserialization_Required : Boolean := True;
   --  Indicates whether the generated image functions are actually
   --  to output the data, or whether an empty string is required.
   --
   --  Note, this is a static value,so if it is false GNAT will emit
   --  no image-generation code. This is a Good Idea both to reduce
   --  the size of a target executable and also to ensure it doesn't
   --  contain code that will never be executed.

package ColdFrame.Serialization_Signature is

   subtype Base is Actual_Base;

   Deserialize : constant Boolean := Deserialization_Required;

end ColdFrame.Serialization_Signature;
