--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to support serialization.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-serialization.ads,v $
--  $Revision: 4214c4b8ed81 $
--  $Date: 2003/01/26 16:32:24 $
--  $Author: simon $

with ColdFrame.Serialization_Signature;
with ColdFrame.Serialization;
package ColdFrame.Project.Serialization
is new ColdFrame.Serialization_Signature
  (Actual_Base => ColdFrame.Serialization.Base,
   Actual_Image => ColdFrame.Serialization.Class_Image);
