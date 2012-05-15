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

with Association_Classes.A0;
with Association_Classes.A1;
with Association_Classes.A2;
with Association_Classes.A3;
with Association_Classes.A4;
with Association_Classes.A5_Association;
with Association_Classes.A6;
with Association_Classes.A7_Association;
with Association_Classes.A8;

with Association_Classes.L.Collections;
with Association_Classes.R.Collections;
with Association_Classes.Initialize;
with Association_Classes.Tear_Down;

package body Association_Classes.Test is

   procedure Navigate_A0_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A0_From_Null_L (C : in out Test_Case'Class) is
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A0.A0L (A_L => null);
      Assert (C,
              RH = null,
              "expected null R handle");
   end Navigate_A0_From_Null_L;

   procedure Navigate_A0_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A0_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A0.A0R (A_R => null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A0_From_Null_R;

   procedure Navigate_A1_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A1_From_Null_L (C : in out Test_Case'Class) is
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A1.A1L (A_L => null);
      Assert (C,
              RH = null,
              "expected null R handle");
   end Navigate_A1_From_Null_L;

   procedure Navigate_A1_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A1_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A1.A1R (A_R => null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A1_From_Null_R;

   procedure Navigate_A2_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A2_From_Null_L (C : in out Test_Case'Class) is
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A2.A2L (A_L => null);
      Assert (C,
              RH = null,
              "expected null R handle");
   end Navigate_A2_From_Null_L;

   procedure Navigate_A2_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A2_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A2.A2R (A_R => null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A2_From_Null_R;

   procedure Navigate_A3_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A3_From_Null_L (C : in out Test_Case'Class) is
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A3.A3L (A_L => null);
      Assert (C,
              RH = null,
              "expected null R handle");
   end Navigate_A3_From_Null_L;

   procedure Navigate_A3_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A3_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A3.A3R (A_R => null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A3_From_Null_R;

   procedure Navigate_A4_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A4_From_Null_L (C : in out Test_Case'Class) is
      RC : R.Collections.Collection;
   begin
      RC := A4.A4L (A_L => null);
      Assert (C,
              R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A4_From_Null_L;

   procedure Navigate_A4_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A4_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A4.A4R (A_R => null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A4_From_Null_R;

   procedure Navigate_A5_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A5_From_Null_L (C : in out Test_Case'Class) is
      RC : R.Collections.Collection;
   begin
      RC := A5_Association.A5L (A_L => null);
      Assert (C,
              R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A5_From_Null_L;

   procedure Navigate_A5_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A5_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A5_Association.A5R (A_R => null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A5_From_Null_R;

   procedure Navigate_A6_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A6_From_Null_L (C : in out Test_Case'Class) is
      RC : R.Collections.Collection;
   begin
      RC := A6.A6L (A_L => null);
      Assert (C,
              R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A6_From_Null_L;

   procedure Navigate_A6_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A6_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A6.A6R (A_R => null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A6_From_Null_R;

   procedure Navigate_A7_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A7_From_Null_L (C : in out Test_Case'Class) is
      RC : R.Collections.Collection;
   begin
      RC := A7_Association.A7L (A_L => null);
      Assert (C,
              R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A7_From_Null_L;

   procedure Navigate_A7_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A7_From_Null_R (C : in out Test_Case'Class) is
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A7_Association.A7R (A_R => null);
      Assert (C,
              LH = null,
              "expected null L handle");
   end Navigate_A7_From_Null_R;

   procedure Navigate_A8_From_Null_L (C : in out Test_Case'Class);
   procedure Navigate_A8_From_Null_L (C : in out Test_Case'Class) is
      RC : R.Collections.Collection;
   begin
      RC := A8.A8L (A_L => null);
      Assert (C,
              R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A8_From_Null_L;

   procedure Navigate_A8_From_Null_R (C : in out Test_Case'Class);
   procedure Navigate_A8_From_Null_R (C : in out Test_Case'Class) is
      LC : L.Collections.Collection;
   begin
      LC := A8.A8R (A_R => null);
      Assert (C,
              L.Collections.Length (LC) = 0,
              "expected empty L collection");
   end Navigate_A8_From_Null_R;

   function Name (C : Case_1) return AUnit.Message_String is
      pragma Unreferenced (C);
   begin
      return new String'("SF_3086637 Association_Classes.Case_1");
   end Name;

   procedure Register_Tests (C : in out Case_1) is
   begin
      Registration.Register_Routine
        (C,
         Navigate_A0_From_Null_L'Access,
         "can navigate 1:1 from null L");
      Registration.Register_Routine
        (C,
         Navigate_A0_From_Null_R'Access,
         "can navigate 1:1 from null R");
      Registration.Register_Routine
        (C,
         Navigate_A1_From_Null_L'Access,
         "can navigate 1c:1c from null L");
      Registration.Register_Routine
        (C,
         Navigate_A1_From_Null_R'Access,
         "can navigate 1c:1c from null R");
      Registration.Register_Routine
        (C,
         Navigate_A2_From_Null_L'Access,
         "can navigate 1c:1 from null");
      Registration.Register_Routine
        (C,
         Navigate_A2_From_Null_R'Access,
         "can navigate 1:1c from null");
      Registration.Register_Routine
        (C,
         Navigate_A3_From_Null_L'Access,
         "can navigate 1:1c from null");
      Registration.Register_Routine
        (C,
         Navigate_A3_From_Null_R'Access,
         "can navigate 1c:1 from null");
      Registration.Register_Routine
        (C,
         Navigate_A4_From_Null_L'Access,
         "can navigate 1c:Mc from null");
      Registration.Register_Routine
        (C,
         Navigate_A4_From_Null_R'Access,
         "can navigate Mc:1c from null");
      Registration.Register_Routine
        (C,
         Navigate_A5_From_Null_L'Access,
         "can navigate 1c:M from null");
      Registration.Register_Routine
        (C,
         Navigate_A5_From_Null_R'Access,
         "can navigate M:1c from null");
      Registration.Register_Routine
        (C,
         Navigate_A6_From_Null_L'Access,
         "can navigate 1:Mc from null");
      Registration.Register_Routine
        (C,
         Navigate_A6_From_Null_R'Access,
         "can navigate Mc:1 from null");
      Registration.Register_Routine
        (C,
         Navigate_A7_From_Null_L'Access,
         "can navigate 1:M from null");
      Registration.Register_Routine
        (C,
         Navigate_A7_From_Null_R'Access,
         "can navigate M:1 from null");
      Registration.Register_Routine
        (C,
         Navigate_A8_From_Null_L'Access,
         "can navigate Mc:Mc from null");
      Registration.Register_Routine
        (C,
         Navigate_A8_From_Null_R'Access,
         "can navigate Mc:Mc from null");
   end Register_Tests;

   procedure Set_Up (C : in out Case_1) is
      pragma Unreferenced (C);
   begin
      Association_Classes.Initialize;
   end Set_Up;

   procedure Tear_Down (C : in out Case_1) is
      pragma Unreferenced (C);
   begin
      Association_Classes.Tear_Down;
   end Tear_Down;

end Association_Classes.Test;
