--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to provide the storage
--  pool used to support classes whose multiplicity isn't specified.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-global_storage_pool.ads,v $
--  $Revision: c2203da97189 $
--  $Date: 2002/09/15 10:29:59 $
--  $Author: simon $

--  with BC.Support.Standard_Storage;
with GNAT.Debug_Pools;
with System.Storage_Pools;

package ColdFrame.Project.Global_Storage_Pool is

--     Pool : System.Storage_Pools.Root_Storage_Pool'Class
--       renames BC.Support.Standard_Storage.Pool;

   The_Pool : GNAT.Debug_Pools.Debug_Pool;
   type T is access Integer;
   for T'Storage_Pool use The_Pool;
   Pool : System.Storage_Pools.Root_Storage_Pool'Class
     renames T'Storage_Pool;

end ColdFrame.Project.Global_Storage_Pool;
