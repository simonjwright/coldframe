--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this procedure to exist to allow errors to be
--  reported (for example, exceptions during domain Initialize
--  procedures).
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-log_info.ads,v $
--  $Revision: 0342c7e00b63 $
--  $Date: 2007/10/27 12:40:37 $
--  $Author: simonjwright $

procedure ColdFrame.Project.Log_Info (Message : String);
pragma Elaborate_Body (ColdFrame.Project.Log_Info);
