--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  $RCSfile: coldframe-project-serialization.adb,v $
--  $Revision: d9e98b17dad2 $
--  $Date: 2003/01/24 07:03:28 $
--  $Author: simon $

package body ColdFrame.Project.Serialization is


   function Image (S : Base'Class) return String is
   begin
      --  force the dispatch
      return ColdFrame.Serialization.Image (S);
   end Image;


end ColdFrame.Project.Serialization;
