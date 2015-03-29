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

procedure ColdFrame.Project.Log_Error (Message : String);
pragma Elaborate_Body (ColdFrame.Project.Log_Error);
