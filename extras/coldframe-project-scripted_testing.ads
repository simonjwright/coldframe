--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This is ColdFrame's default implementation.

with ColdFrame.Scripted_Testing_G;
with ColdFrame.Project.Events;
package ColdFrame.Project.Scripted_Testing
is new ColdFrame.Scripted_Testing_G (Events => ColdFrame.Project.Events);
