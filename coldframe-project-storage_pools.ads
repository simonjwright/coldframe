--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to provide the storage
--  pool used to support
--
--   o  classes whose multiplicity is specified (Bounded_Pool).
--   o  classes whose multiplicity isn't specified (Unbounded_Pool).
--   o  event queues (Unbounded_Pool).
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-storage_pools.ads,v $
--  $Revision: 204f7648b0fb $
--  $Date: 2003/07/24 21:03:43 $
--  $Author: simon $

with ColdFrame.Bounded_Storage_Pools;
with ColdFrame.Unbounded_Storage_Pools;
with System.Storage_Pools;

package ColdFrame.Project.Storage_Pools is

   subtype Bounded_Pool
      is Bounded_Storage_Pools.Bounded_Pool;

   Unbounded_Pool : Unbounded_Storage_Pools.Unbounded_Pool;

   Pool : System.Storage_Pools.Root_Storage_Pool'Class
     renames System.Storage_Pools.Root_Storage_Pool'Class
                (Unbounded_Pool);

end ColdFrame.Project.Storage_Pools;
