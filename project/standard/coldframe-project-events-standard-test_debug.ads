--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports unit testing of code involving events, with
--  tracing of event processing.
--
--  This is provided to retain the obsolete interface of Release
--  20040319 and earlier.

with ColdFrame.Project.Events.Standard.Test_Trace;

package ColdFrame.Project.Events.Standard.Test_Debug
renames ColdFrame.Project.Events.Standard.Test_Trace;
