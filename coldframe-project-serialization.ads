--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  $RCSfile: coldframe-project-serialization.ads,v $
--  $Revision: d9e98b17dad2 $
--  $Date: 2003/01/24 07:03:28 $
--  $Author: simon $

with ColdFrame.Serialization;

package ColdFrame.Project.Serialization is

   pragma Elaborate_Body;

   --  This package provides off-the-shelf support for {serializable}
   --  <<type>> classes.
   --
   --  The Ada translation of <<type, serializable>> Foo (which must
   --  be a record type, ie have attributes) is
   --
   --     type Foo is new ColdFrame.Project.Serialization.Base with record
   --        ..
   --     end record;

   subtype Base is ColdFrame.Serialization.Base;

   function Image (S : Base'Class) return String;
   --  This function dispatches to the generated Image function (there
   --  is no way to rename the primitive Image).

end ColdFrame.Project.Serialization;
