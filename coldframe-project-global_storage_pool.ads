--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to provide the storage
--  pool used to support
--
--   o  classes whose multiplicity isn't specified
--   o  event queues.
--
--  This is ColdFrame's default implementation.
--
--  See also coldframe-project-global_storage_pool.ads-debug for an
--  alternative using GNAT Debug Pools.

--  $RCSfile: coldframe-project-global_storage_pool.ads,v $
--  $Revision: b720f64f3037 $
--  $Date: 2002/09/28 17:13:37 $
--  $Author: simon $

with BC.Support.Standard_Storage;
with System.Storage_Pools;

package ColdFrame.Project.Global_Storage_Pool is

   Pool : System.Storage_Pools.Root_Storage_Pool'Class
     renames BC.Support.Standard_Storage.Pool;

end ColdFrame.Project.Global_Storage_Pool;
