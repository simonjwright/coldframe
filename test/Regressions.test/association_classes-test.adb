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

--  SF 3086637

with AUnit.Assertions; use AUnit.Assertions;

with Association_Classes.A0;
with Association_Classes.A0_Class;
with Association_Classes.A1;
with Association_Classes.A1_Class;
with Association_Classes.A2;
with Association_Classes.A2_Class;
with Association_Classes.A3;
with Association_Classes.A3_Class;
with Association_Classes.A4;
with Association_Classes.A4_Class.Collections;
with Association_Classes.A5.Collections;
with Association_Classes.A5_Association;
with Association_Classes.A6;
with Association_Classes.A6_Associative.Collections;
with Association_Classes.A7_Association;
with Association_Classes.A7_Associative.Collections;
with Association_Classes.A8;
with Association_Classes.A8_Associative.Collections;

with Association_Classes.L.Collections;
with Association_Classes.R.Collections;
with Association_Classes.Initialize;
with Association_Classes.Tear_Down;

package body Association_Classes.Test is

   procedure Navigate_A0_From_Null_L_To_R (C : in out Test_Case'Class);
   procedure Navigate_A0_From_Null_L_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A0.A0L (A_L => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A0_From_Null_L_To_R;

   procedure Navigate_A0_From_Null_L_To_A (C : in out Test_Case'Class);
   procedure Navigate_A0_From_Null_L_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A0_Class.Handle;
      use type A0_Class.Handle;
   begin
      AH := A0.A0L (A_L => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A0_From_Null_L_To_A;

   procedure Navigate_A0_From_Null_A_To_L (C : in out Test_Case'Class);
   procedure Navigate_A0_From_Null_A_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A0.A0R (AC => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A0_From_Null_A_To_L;

   procedure Navigate_A0_From_Null_R_To_L (C : in out Test_Case'Class);
   procedure Navigate_A0_From_Null_R_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A0.A0R (A_R => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A0_From_Null_R_To_L;

   procedure Navigate_A0_From_Null_R_To_A (C : in out Test_Case'Class);
   procedure Navigate_A0_From_Null_R_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A0_Class.Handle;
      use type A0_Class.Handle;
   begin
      AH := A0.A0R (A_R => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A0_From_Null_R_To_A;

   procedure Navigate_A0_From_Null_A_To_R (C : in out Test_Case'Class);
   procedure Navigate_A0_From_Null_A_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A0.A0L (AC => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A0_From_Null_A_To_R;

   procedure Navigate_A1_From_Null_L_To_R (C : in out Test_Case'Class);
   procedure Navigate_A1_From_Null_L_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A1.A1L (A_L => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A1_From_Null_L_To_R;

   procedure Navigate_A1_From_Null_L_To_A (C : in out Test_Case'Class);
   procedure Navigate_A1_From_Null_L_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A1_Class.Handle;
      use type A1_Class.Handle;
   begin
      AH := A1.A1L (A_L => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A1_From_Null_L_To_A;

   procedure Navigate_A1_From_Null_A_To_L (C : in out Test_Case'Class);
   procedure Navigate_A1_From_Null_A_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A1.A1R (AC => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A1_From_Null_A_To_L;

   procedure Navigate_A1_From_Null_R_To_L (C : in out Test_Case'Class);
   procedure Navigate_A1_From_Null_R_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A1.A1R (A_R => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A1_From_Null_R_To_L;

   procedure Navigate_A1_From_Null_R_To_A (C : in out Test_Case'Class);
   procedure Navigate_A1_From_Null_R_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A1_Class.Handle;
      use type A1_Class.Handle;
   begin
      AH := A1.A1R (A_R => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A1_From_Null_R_To_A;

   procedure Navigate_A1_From_Null_A_To_R (C : in out Test_Case'Class);
   procedure Navigate_A1_From_Null_A_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A1.A1L (AC => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A1_From_Null_A_To_R;

   procedure Navigate_A2_From_Null_L_To_R (C : in out Test_Case'Class);
   procedure Navigate_A2_From_Null_L_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A2.A2L (A_L => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A2_From_Null_L_To_R;

   procedure Navigate_A2_From_Null_L_To_A (C : in out Test_Case'Class);
   procedure Navigate_A2_From_Null_L_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A2_Class.Handle;
      use type A2_Class.Handle;
   begin
      AH := A2.A2L (A_L => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A2_From_Null_L_To_A;

   procedure Navigate_A2_From_Null_A_To_L (C : in out Test_Case'Class);
   procedure Navigate_A2_From_Null_A_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A2.A2R (AC => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A2_From_Null_A_To_L;

   procedure Navigate_A2_From_Null_R_To_L (C : in out Test_Case'Class);
   procedure Navigate_A2_From_Null_R_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A2.A2R (A_R => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A2_From_Null_R_To_L;

   procedure Navigate_A2_From_Null_R_To_A (C : in out Test_Case'Class);
   procedure Navigate_A2_From_Null_R_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A2_Class.Handle;
      use type A2_Class.Handle;
   begin
      AH := A2.A2R (A_R => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A2_From_Null_R_To_A;

   procedure Navigate_A2_From_Null_A_To_R (C : in out Test_Case'Class);
   procedure Navigate_A2_From_Null_A_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A2.A2L (AC => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A2_From_Null_A_To_R;

   procedure Navigate_A3_From_Null_L_To_R (C : in out Test_Case'Class);
   procedure Navigate_A3_From_Null_L_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A3.A3L (A_L => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A3_From_Null_L_To_R;

   procedure Navigate_A3_From_Null_L_To_A (C : in out Test_Case'Class);
   procedure Navigate_A3_From_Null_L_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A3_Class.Handle;
      use type A3_Class.Handle;
   begin
      AH := A3.A3L (A_L => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A3_From_Null_L_To_A;

   procedure Navigate_A3_From_Null_A_To_L (C : in out Test_Case'Class);
   procedure Navigate_A3_From_Null_A_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A3.A3R (AC => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A3_From_Null_A_To_L;

   procedure Navigate_A3_From_Null_R_To_L (C : in out Test_Case'Class);
   procedure Navigate_A3_From_Null_R_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A3.A3R (A_R => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A3_From_Null_R_To_L;

   procedure Navigate_A3_From_Null_R_To_A (C : in out Test_Case'Class);
   procedure Navigate_A3_From_Null_R_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A3_Class.Handle;
      use type A3_Class.Handle;
   begin
      AH := A3.A3R (A_R => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A3_From_Null_R_To_A;

   procedure Navigate_A3_From_Null_A_To_R (C : in out Test_Case'Class);
   procedure Navigate_A3_From_Null_A_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A3.A3L (AC => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A3_From_Null_A_To_R;

   procedure Navigate_A4_From_Null_L_To_R (C : in out Test_Case'Class);
   procedure Navigate_A4_From_Null_L_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RC : R.Collections.Collection;
   begin
      RC := A4.A4L (A_L => null);
      Assert (R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A4_From_Null_L_To_R;

   procedure Navigate_A4_From_Null_L_To_A (C : in out Test_Case'Class);
   procedure Navigate_A4_From_Null_L_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AC : A4_Class.Collections.Collection;
   begin
      AC := A4.A4L (A_L => null);
      Assert (A4_Class.Collections.Length (AC) = 0,
              "expected empty A4_Class collection");
   end Navigate_A4_From_Null_L_To_A;

   procedure Navigate_A4_From_Null_A_To_L (C : in out Test_Case'Class);
   procedure Navigate_A4_From_Null_A_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A4.A4R (AC => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A4_From_Null_A_To_L;

   procedure Navigate_A4_From_Null_R_To_L (C : in out Test_Case'Class);
   procedure Navigate_A4_From_Null_R_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A4.A4R (A_R => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A4_From_Null_R_To_L;

   procedure Navigate_A4_From_Null_R_To_A (C : in out Test_Case'Class);
   procedure Navigate_A4_From_Null_R_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A4_Class.Handle;
      use type A4_Class.Handle;
   begin
      AH := A4.A4R (A_R => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A4_From_Null_R_To_A;

   procedure Navigate_A4_From_Null_A_To_R (C : in out Test_Case'Class);
   procedure Navigate_A4_From_Null_A_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A4.A4L (AC => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A4_From_Null_A_To_R;

   procedure Navigate_A5_From_Null_L_To_R (C : in out Test_Case'Class);
   procedure Navigate_A5_From_Null_L_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RC : R.Collections.Collection;
   begin
      RC := A5_Association.A5L (A_L => null);
      Assert (R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A5_From_Null_L_To_R;

   procedure Navigate_A5_From_Null_L_To_A (C : in out Test_Case'Class);
   procedure Navigate_A5_From_Null_L_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AC : A5.Collections.Collection;
   begin
      AC := A5_Association.A5L (A_L => null);
      Assert (A5.Collections.Length (AC) = 0,
              "expected empty A5 collection");
   end Navigate_A5_From_Null_L_To_A;

   procedure Navigate_A5_From_Null_A_To_L (C : in out Test_Case'Class);
   procedure Navigate_A5_From_Null_A_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A5_Association.A5R (An_A5 => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A5_From_Null_A_To_L;

   procedure Navigate_A5_From_Null_R_To_L (C : in out Test_Case'Class);
   procedure Navigate_A5_From_Null_R_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A5_Association.A5R (A_R => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A5_From_Null_R_To_L;

   procedure Navigate_A5_From_Null_R_To_A (C : in out Test_Case'Class);
   procedure Navigate_A5_From_Null_R_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A5.Handle;
      use type A5.Handle;
   begin
      AH := A5_Association.A5R (A_R => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A5_From_Null_R_To_A;

   procedure Navigate_A5_From_Null_A_To_R (C : in out Test_Case'Class);
   procedure Navigate_A5_From_Null_A_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A5_Association.A5L (An_A5 => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A5_From_Null_A_To_R;

   procedure Navigate_A6_From_Null_L_To_R (C : in out Test_Case'Class);
   procedure Navigate_A6_From_Null_L_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RC : R.Collections.Collection;
   begin
      RC := A6.A6L (A_L => null);
      Assert (R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A6_From_Null_L_To_R;

   procedure Navigate_A6_From_Null_L_To_A (C : in out Test_Case'Class);
   procedure Navigate_A6_From_Null_L_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AC : A6_Associative.Collections.Collection;
   begin
      AC := A6.A6L (A_L => null);
      Assert (A6_Associative.Collections.Length (AC) = 0,
              "expected empty A6_Associative collection");
   end Navigate_A6_From_Null_L_To_A;

   procedure Navigate_A6_From_Null_A_To_L (C : in out Test_Case'Class);
   procedure Navigate_A6_From_Null_A_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A6.A6R (AA => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A6_From_Null_A_To_L;

   procedure Navigate_A6_From_Null_R_To_L (C : in out Test_Case'Class);
   procedure Navigate_A6_From_Null_R_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A6.A6R (A_R => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A6_From_Null_R_To_L;

   procedure Navigate_A6_From_Null_R_To_A (C : in out Test_Case'Class);
   procedure Navigate_A6_From_Null_R_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A6_Associative.Handle;
      use type A6_Associative.Handle;
   begin
      AH := A6.A6R (A_R => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A6_From_Null_R_To_A;

   procedure Navigate_A6_From_Null_A_To_R (C : in out Test_Case'Class);
   procedure Navigate_A6_From_Null_A_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A6.A6L (AA => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A6_From_Null_A_To_R;

   procedure Navigate_A7_From_Null_L_To_R (C : in out Test_Case'Class);
   procedure Navigate_A7_From_Null_L_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RC : R.Collections.Collection;
   begin
      RC := A7_Association.A7L (A_L => null);
      Assert (R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A7_From_Null_L_To_R;

   procedure Navigate_A7_From_Null_L_To_A (C : in out Test_Case'Class);
   procedure Navigate_A7_From_Null_L_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AC : A7_Associative.Collections.Collection;
   begin
      AC := A7_Association.A7L (A_L => null);
      Assert (A7_Associative.Collections.Length (AC) = 0,
              "expected empty A7_Associative collection");
   end Navigate_A7_From_Null_L_To_A;

   procedure Navigate_A7_From_Null_A_To_L (C : in out Test_Case'Class);
   procedure Navigate_A7_From_Null_A_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A7_Association.A7R (AA => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A7_From_Null_A_To_L;

   procedure Navigate_A7_From_Null_R_To_L (C : in out Test_Case'Class);
   procedure Navigate_A7_From_Null_R_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A7_Association.A7R (A_R => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A7_From_Null_R_To_L;

   procedure Navigate_A7_From_Null_R_To_A (C : in out Test_Case'Class);
   procedure Navigate_A7_From_Null_R_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AH : A7_Associative.Handle;
      use type A7_Associative.Handle;
   begin
      AH := A7_Association.A7R (A_R => null);
      Assert (AH = null,
              "expected null A handle");
   end Navigate_A7_From_Null_R_To_A;

   procedure Navigate_A7_From_Null_A_To_R (C : in out Test_Case'Class);
   procedure Navigate_A7_From_Null_A_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A7_Association.A7L (AA => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A7_From_Null_A_To_R;

   procedure Navigate_A8_From_Null_L_To_R (C : in out Test_Case'Class);
   procedure Navigate_A8_From_Null_L_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RC : R.Collections.Collection;
   begin
      RC := A8.A8L (A_L => null);
      Assert (R.Collections.Length (RC) = 0,
              "expected empty R collection");
   end Navigate_A8_From_Null_L_To_R;

   procedure Navigate_A8_From_Null_L_To_A (C : in out Test_Case'Class);
   procedure Navigate_A8_From_Null_L_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AC : A8_Associative.Collections.Collection;
   begin
      AC := A8.A8L (A_L => null);
      Assert (A8_Associative.Collections.Length (AC) = 0,
              "expected empty A8_Associative collection");
   end Navigate_A8_From_Null_L_To_A;

   procedure Navigate_A8_From_Null_A_To_L (C : in out Test_Case'Class);
   procedure Navigate_A8_From_Null_A_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LH : L.Handle;
      use type L.Handle;
   begin
      LH := A8.A8R (AA => null);
      Assert (LH = null,
              "expected null L handle");
   end Navigate_A8_From_Null_A_To_L;

   procedure Navigate_A8_From_Null_R_To_L (C : in out Test_Case'Class);
   procedure Navigate_A8_From_Null_R_To_L (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      LC : L.Collections.Collection;
   begin
      LC := A8.A8R (A_R => null);
      Assert (L.Collections.Length (LC) = 0,
              "expected empty L collection");
   end Navigate_A8_From_Null_R_To_L;

   procedure Navigate_A8_From_Null_R_To_A (C : in out Test_Case'Class);
   procedure Navigate_A8_From_Null_R_To_A (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      AC : A8_Associative.Collections.Collection;
   begin
      AC := A8.A8R (A_R => null);
      Assert (A8_Associative.Collections.Length (AC) = 0,
              "expected empty A8_Associative collection");
   end Navigate_A8_From_Null_R_To_A;

   procedure Navigate_A8_From_Null_A_To_R (C : in out Test_Case'Class);
   procedure Navigate_A8_From_Null_A_To_R (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      RH : R.Handle;
      use type R.Handle;
   begin
      RH := A8.A8L (AA => null);
      Assert (RH = null,
              "expected null R handle");
   end Navigate_A8_From_Null_A_To_R;

   function Name (C : Case_1) return AUnit.Message_String is
      pragma Unreferenced (C);
   begin
      return new String'("SF_3086637 Association_Classes.Case_1");
   end Name;

   procedure Register_Tests (C : in out Case_1) is
   begin
      Registration.Register_Routine
        (C,
         Navigate_A0_From_Null_L_To_R'Access,
         "can navigate 1:1 from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A0_From_Null_L_To_A'Access,
         "can navigate 1:1 from null L to A");
      Registration.Register_Routine
        (C,
         Navigate_A0_From_Null_A_To_L'Access,
         "can navigate 1:1 from null A to L");
      Registration.Register_Routine
        (C,
         Navigate_A0_From_Null_R_To_L'Access,
         "can navigate 1:1 from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A0_From_Null_R_To_A'Access,
         "can navigate 1:1 from null R to A");
      Registration.Register_Routine
        (C,
         Navigate_A0_From_Null_A_To_R'Access,
         "can navigate 1:1 from null A to R");
      Registration.Register_Routine
        (C,
         Navigate_A1_From_Null_L_To_R'Access,
         "can navigate 1c:1c from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A1_From_Null_L_To_A'Access,
         "can navigate 1c:1c from null L to A");
      Registration.Register_Routine
        (C,
         Navigate_A1_From_Null_A_To_L'Access,
         "can navigate 1c:1c from null A to L");
      Registration.Register_Routine
        (C,
         Navigate_A1_From_Null_R_To_L'Access,
         "can navigate 1c:1c from null R");
      Registration.Register_Routine
        (C,
         Navigate_A1_From_Null_R_To_A'Access,
         "can navigate 1c:1c from null R to A");
      Registration.Register_Routine
        (C,
         Navigate_A1_From_Null_A_To_R'Access,
         "can navigate 1c:1c from null A to R");
      Registration.Register_Routine
        (C,
         Navigate_A2_From_Null_L_To_R'Access,
         "can navigate 1c:1 from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A2_From_Null_L_To_A'Access,
         "can navigate 1c:1 from null L to A");
      Registration.Register_Routine
        (C,
         Navigate_A2_From_Null_A_To_L'Access,
         "can navigate 1c:1 from null A to L");
      Registration.Register_Routine
        (C,
         Navigate_A2_From_Null_R_To_L'Access,
         "can navigate 1c:1 from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A2_From_Null_R_To_A'Access,
         "can navigate 1c:1 from null R to A");
      Registration.Register_Routine
        (C,
         Navigate_A2_From_Null_A_To_R'Access,
         "can navigate 1c:1 from null A to R");
      Registration.Register_Routine
        (C,
         Navigate_A3_From_Null_L_To_R'Access,
         "can navigate 1:1c from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A3_From_Null_L_To_A'Access,
         "can navigate 1:1c from null L to A");
      Registration.Register_Routine
        (C,
         Navigate_A3_From_Null_A_To_L'Access,
         "can navigate 1:1c from null A to L");
      Registration.Register_Routine
        (C,
         Navigate_A3_From_Null_R_To_L'Access,
         "can navigate 1:1c from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A3_From_Null_R_To_A'Access,
         "can navigate 1:1c from null R to A");
      Registration.Register_Routine
        (C,
         Navigate_A3_From_Null_A_To_R'Access,
         "can navigate 1:1c from null A to R");
      Registration.Register_Routine
        (C,
         Navigate_A4_From_Null_L_To_R'Access,
         "can navigate 1c:Mc from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A4_From_Null_L_To_A'Access,
         "can navigate 1:1c from null L to A");
      Registration.Register_Routine
        (C,
         Navigate_A4_From_Null_A_To_L'Access,
         "can navigate 1:1c from null A to L");
      Registration.Register_Routine
        (C,
         Navigate_A4_From_Null_R_To_L'Access,
         "can navigate 1c:Mc from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A4_From_Null_R_To_A'Access,
         "can navigate 1:1c from null R to A");
      Registration.Register_Routine
        (C,
         Navigate_A4_From_Null_A_To_R'Access,
         "can navigate 1:1c from null A to R");
      Registration.Register_Routine
        (C,
         Navigate_A5_From_Null_L_To_R'Access,
         "can navigate 1c:M from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A5_From_Null_L_To_A'Access,
         "can navigate 1c:M from null L to A");
      Registration.Register_Routine
        (C,
         Navigate_A5_From_Null_A_To_L'Access,
         "can navigate 1c:M from null A to L");
      Registration.Register_Routine
        (C,
         Navigate_A5_From_Null_R_To_L'Access,
         "can navigate 1c:M from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A5_From_Null_R_To_A'Access,
         "can navigate 1c:M from null R to A");
      Registration.Register_Routine
        (C,
         Navigate_A5_From_Null_A_To_R'Access,
         "can navigate 1c:M from null A to R");
      Registration.Register_Routine
        (C,
         Navigate_A6_From_Null_L_To_R'Access,
         "can navigate 1:Mc from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A6_From_Null_L_To_A'Access,
         "can navigate 1:Mc from null L to A");
      Registration.Register_Routine
        (C,
         Navigate_A6_From_Null_A_To_L'Access,
         "can navigate 1:Mc from null A to L");
      Registration.Register_Routine
        (C,
         Navigate_A6_From_Null_R_To_L'Access,
         "can navigate 1:Mc from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A6_From_Null_R_To_A'Access,
         "can navigate 1:Mc from null R to A");
      Registration.Register_Routine
        (C,
         Navigate_A6_From_Null_A_To_R'Access,
         "can navigate 1:Mc from null A to R");
      Registration.Register_Routine
        (C,
         Navigate_A7_From_Null_L_To_R'Access,
         "can navigate 1:M from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A7_From_Null_L_To_A'Access,
         "can navigate 1:M from null L to A");
      Registration.Register_Routine
        (C,
         Navigate_A7_From_Null_A_To_L'Access,
         "can navigate 1:M from null A to L");
      Registration.Register_Routine
        (C,
         Navigate_A7_From_Null_R_To_L'Access,
         "can navigate 1:M from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A7_From_Null_R_To_A'Access,
         "can navigate 1:M from null R to A");
      Registration.Register_Routine
        (C,
         Navigate_A7_From_Null_A_To_R'Access,
         "can navigate 1:M from null A to R");
      Registration.Register_Routine
        (C,
         Navigate_A8_From_Null_L_To_R'Access,
         "can navigate Mc:Mc from null L to R");
      Registration.Register_Routine
        (C,
         Navigate_A8_From_Null_L_To_A'Access,
         "can navigate Mc:Mc from null L to A");
      Registration.Register_Routine
        (C,
         Navigate_A8_From_Null_A_To_L'Access,
         "can navigate Mc:Mc from null A to L");
      Registration.Register_Routine
        (C,
         Navigate_A8_From_Null_R_To_L'Access,
         "can navigate Mc:Mc from null R to L");
      Registration.Register_Routine
        (C,
         Navigate_A8_From_Null_R_To_A'Access,
         "can navigate Mc:Mc from null R to A");
      Registration.Register_Routine
        (C,
         Navigate_A8_From_Null_A_To_R'Access,
         "can navigate Mc:Mc from null A to R");
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
