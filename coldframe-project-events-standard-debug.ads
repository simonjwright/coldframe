--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports tracing of code involving events.
--
--  This is provided to retain the obsolete interface of Release
--  20040319 and earlier.

--  $RCSfile: coldframe-project-events-standard-debug.ads,v $
--  $Revision: 6ea040caff18 $
--  $Date: 2004/10/09 10:37:13 $
--  $Author: simon $

with ColdFrame.Project.Events.Standard.Trace;

package ColdFrame.Project.Events.Standard.Debug
renames ColdFrame.Project.Events.Standard.Trace;
