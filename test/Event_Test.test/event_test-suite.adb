--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Event_Test.Test_Class;
with Event_Test.Test_Completion_Transitions;
with Event_Test.Test_Engine;
with Event_Test.Test_Inspection;
with Event_Test.Test_Instance;
with Event_Test.Test_Queue;
with Event_Test.Test_Singleton_Instance;
with Event_Test.Test_Timer_Task_Teardown;

function Event_Test.Suite return AUnit.Test_Suites.Access_Test_Suite is
   use AUnit.Test_Suites;
   Result : constant Access_Test_Suite := new Test_Suite;
begin
   Add_Test (Result, new Test_Queue.Test_Case);
   Add_Test (Result, new Test_Engine.Test_Case);
   Add_Test (Result, new Test_Instance.Test_Case);
   Add_Test (Result, new Test_Singleton_Instance.Test_Case);
   Add_Test (Result, new Test_Class.Test_Case);
   Add_Test (Result, new Test_Completion_Transitions.Test_Case);
   Add_Test (Result, new Test_Timer_Task_Teardown.Test_Case);
   Add_Test (Result, new Test_Inspection.Test_Case);
   return Result;
end Event_Test.Suite;
