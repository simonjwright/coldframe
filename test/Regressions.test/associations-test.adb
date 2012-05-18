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

with Associations.A0;
with Associations.A1;
with Associations.A2;
with Associations.A3;
with Associations.A4;
with Associations.A5;
with Associations.A6;
with Associations.A7;

with Associations.L;
with Associations.R.Collections;
with Associations.Initialize;
with Associations.Tear_Down;

package body Associations.Test is

   procedure Navigate_A0_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A0_From_Null_L (C : in out Test_Case'Class) is
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A0.A0L (null);
      Assert (C,
              RH = null,
              "expected null R handle");
   end Navigate_A0_From_Null_L;

   procedure Navigate_A0_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A0_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A0.A0R (null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A0_From_Null_R;

   procedure Navigate_A1_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A1_From_Null_L (C : in out Test_Case'Class) is
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A1.A1L (null);
      Assert (C,
              RH = null,
              "expected null R handle");
   end Navigate_A1_From_Null_L;

   procedure Navigate_A1_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A1_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A1.A1R (null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A1_From_Null_R;

   procedure Navigate_A2_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A2_From_Null_L (C : in out Test_Case'Class) is
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A2.A2L (null);
      Assert (C,
              RH = null,
              "expected null R handle");
   end Navigate_A2_From_Null_L;

   procedure Navigate_A2_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A2_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A2.A2R (null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A2_From_Null_R;

   procedure Navigate_A3_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A3_From_Null_L (C : in out Test_Case'Class) is
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A3.A3L (null);
      Assert (C,
              RH = null,
              "expected null R handle");
   end Navigate_A3_From_Null_L;

   procedure Navigate_A3_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A3_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A3.A3R (null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A3_From_Null_R;

   procedure Navigate_A4_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A4_From_Null_L (C : in out Test_Case'Class) is
      RC : R.Collections.Collection;
   begin
      RC := A4.A4L (null);
      Assert (C,
              R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A4_From_Null_L;

   procedure Navigate_A4_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A4_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A4.A4R (null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A4_From_Null_R;

   procedure Navigate_A5_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A5_From_Null_L (C : in out Test_Case'Class) is
      RC : R.Collections.Collection;
   begin
      RC := A5.A5L (null);
      Assert (C,
              R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A5_From_Null_L;

   procedure Navigate_A5_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A5_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A5.A5R (null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A5_From_Null_R;

   procedure Navigate_A6_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A6_From_Null_L (C : in out Test_Case'Class) is
      RC : R.Collections.Collection;
   begin
      RC := A6.A6L (null);
      Assert (C,
              R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A6_From_Null_L;

   procedure Navigate_A6_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A6_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A6.A6R (null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A6_From_Null_R;

   procedure Navigate_A7_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A7_From_Null_L (C : in out Test_Case'Class) is
      RC : R.Collections.Collection;
   begin
      RC := A7.A7L (null);
      Assert (C,
              R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A7_From_Null_L;

   procedure Navigate_A7_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A7_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A7.A7R (null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A7_From_Null_R;

   function Name (C : Case_1) return AUnit.Message_String is
      pragma Unreferenced (C);
   begin
      return new String'("SF_3086637 Associations.Case_1");
   end Name;

   procedure Register_Tests (C : in out Case_1) is
   begin
      Registration.Register_Routine
        (C,
         Navigate_A0_From_Null_L'Access,
         "can navigate 1:1 from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A0_From_Null_R'Access,
         "can navigate 1:1 from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A1_From_Null_L'Access,
         "can navigate 1c:1c from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A1_From_Null_R'Access,
         "can navigate 1c:1c from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A2_From_Null_L'Access,
         "can navigate 1c:1 from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A2_From_Null_R'Access,
         "can navigate 1c:1 from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A3_From_Null_L'Access,
         "can navigate 1:1c from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A3_From_Null_R'Access,
         "can navigate 1:1c from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A4_From_Null_L'Access,
         "can navigate 1c:Mc from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A4_From_Null_R'Access,
         "can navigate 1c:Mc from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A5_From_Null_L'Access,
         "can navigate 1c:M from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A5_From_Null_R'Access,
         "can navigate 1c:M from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A6_From_Null_L'Access,
         "can navigate 1:Mc from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A6_From_Null_R'Access,
         "can navigate 1:Mc from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A7_From_Null_L'Access,
         "can navigate 1:M from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A7_From_Null_R'Access,
         "can navigate 1:M from null R to L");
   end Register_Tests;

   procedure Set_Up (C : in out Case_1) is
      pragma Unreferenced (C);
   begin
      Associations.Initialize;
   end Set_Up;

   procedure Tear_Down (C : in out Case_1) is
      pragma Unreferenced (C);
   begin
      Associations.Tear_Down;
   end Tear_Down;

end Associations.Test;
