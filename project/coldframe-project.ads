--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

package ColdFrame.Project is

   --  ColdFrame requires certain project-specific instantiations of
   --  support software to be made available in a package hierarchy
   --  rooted at this package.
   --
   --  Specifically,
   --
   --  ColdFrame.Project.Calendar provides the type Time used to
   --  translate attributes and parameters of type Date or Time, the
   --  function Clock used to generate default return values, and the
   --  function Image used for (de)serialization
   --
   --  ColdFrame.Project.Events must contain an instantiation of
   --  ColdFrame.Events_G
   --
   --  ColdFrame.Project.Log_Error must be an error-logging procedure
   --
   --  ColdFrame.Project.Serialization must provide an abstract tagged
   --  type Base and operations Image, Base_Attribute_Image as in
   --  ColdFrame.Serialization
   --
   --  ColdFrame.Project.Storage_Pools must contain
   --  * an object Pool, used for unbounded allocations, of type
   --    System.Storage_Pools.Root_Storage_Pool'Class
   --  * a (sub)type Bounded_Pool, used for bounded allocations, like
   --      type Bounded_Pool
   --        (Pool_Size : System.Storage_Elements.Storage_Count;
   --         Elmt_Size : System.Storage_Elements.Storage_Count;
   --         Alignment : System.Storage_Elements.Storage_Count)
   --      is new System.Storage_Pools.Root_Storage_Pool with private;

end ColdFrame.Project;
