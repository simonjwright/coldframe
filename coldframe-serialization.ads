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

--  $RCSfile: coldframe-serialization.ads,v $
--  $Revision: 4ef2e51b1e89 $
--  $Date: 2003/03/04 22:05:08 $
--  $Author: simon $

package ColdFrame.Serialization is

   --  This package provides off-the-shelf support for {serializable}
   --  <<type>> classes.
   --
   --  The Ada translation of <<type, serializable>> Foo is, firstly,
   --  a normal declaration of the type in the Domain package. Then in
   --  the Domain.Serializable package, there is
   --
   --     type Foo is new ColdFrame.Project.Serialization.Base with record
   --        Payload : Domain.Foo;
   --     end record;
   --
   --     function Image (S : Foo) return String;
   --
   --  where Image generates an XML representation of Foo.

   type Base is abstract tagged private;

   function Image (S : Base) return String;
   --  Provides a string representation of S.
   --
   --  The default implementation generates a contentless XML element
   --  containing the external tag of the actual type.

   function Class_Image (S : Base'Class) return String;
   --  This function dispatches to the generated Image function (there
   --  is no way to invoke the primitive Image directly).

private

   type Base is abstract tagged null record;

end ColdFrame.Serialization;
