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
--  $Revision: f93ee5ec8abd $
--  $Date: 2002/01/27 11:12:50 $
--  $Author: simon $

package ColdFrame.Instances is

   type Instance_Base is abstract tagged limited private;
   --  All Instances are derived from this type.
   --
   --  The purpose is to allow mutual visibility (to support
   --  associations and inheritance relationships) without using
   --  non-standard extensions such as WITH TYPE.

   type Handle is access all Instance_Base'Class;

private

   type Instance_Base is abstract tagged limited null record;

end ColdFrame.Instances;
