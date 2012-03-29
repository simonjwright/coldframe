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

--  Regression tests for ColdFrame.

with SF_2991721.Cls;
with SF_2991721.Initialize;
with SF_2991721.Tear_Down;

package body SF_2991721.Test is

   procedure Create_Instance (C : in out Test_Case'Class);
   procedure Create_Instance (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      H : SF_2991721.Cls.Handle;
      pragma Unreferenced (H);
   begin
      H := SF_2991721.Cls.Create ((I => -1,
                                   P => 1));
   end Create_Instance;

   function Name (C : Case_1) return AUnit.Message_String is
      pragma Unreferenced (C);
   begin
      return new String'("SF_2991721.Case_1");
   end Name;

   procedure Register_Tests (C : in out Case_1) is
   begin
      Registration.Register_Routine
        (C,
         Create_Instance'Access,
         "can create an instance with a negative integer <<id>> component");
   end Register_Tests;

   procedure Set_Up (C : in out Case_1) is
      pragma Unreferenced (C);
   begin
      SF_2991721.Initialize;
   end Set_Up;

   procedure Tear_Down (C : in out Case_1) is
      pragma Unreferenced (C);
   begin
      SF_2991721.Tear_Down;
   end Tear_Down;

end SF_2991721.Test;
