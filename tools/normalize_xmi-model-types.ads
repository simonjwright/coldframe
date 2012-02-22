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

--  $RCSfile: normalize_xmi-model-types.ads,v $
--  $Revision: c3a01e5d21e1 $
--  $Date: 2012/02/09 17:17:31 $
--  $Author: simonjwright $

private package Normalize_XMI.Model.Types is

   --  This abstract type exists to support access-to-type types.
   --
   --  Each type allows the declaration of one additional type which
   --  provides pointers (accesses) to the type. In old ColdFrame,
   --  this was managed by allowing a tag {access =
   --  name-of-access-type}, but since with ArgoUML this would need to
   --  be accompanied by a stereotype it seemed that things would get
   --  rather crowded; and in any case the access type needs to be
   --  visible in the model, or you can't create parameters or
   --  attributes of the type.
   --
   --  Instead, things have been reversed; we create a DataType,
   --  stereotyped <<access>>, with a tag {access-to-type =
   --  accessed-type-name}. If we find one of these, we set a
   --  reference to the <<access>> type in the designated type.
   --
   --  To be extended by all the type-specifying packages:
   --  Class_Types, Data_Types, Enumerations.
   type Type_Element is abstract new Element with record
      Accessor : Element_P;
   end record;

end Normalize_XMI.Model.Types;
