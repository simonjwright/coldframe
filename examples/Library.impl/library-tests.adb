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

with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Assertions; use AUnit.Assertions;
with ColdFrame.Exceptions;

with Library.Initialize;
with Library.Tear_Down;
with Library.Book.All_Instances;
with Library.Borrower.All_Instances;
with Library.Current;
with Library.History;

with Library.Authorship.From_Vectors;

package body Library.Tests is

   pragma Style_Checks (Off);

   function "+" (S : String) return Unbounded_String
     renames To_Unbounded_String;

   Alice, Bob, Carol, Dave : Borrower.Handle;
   Glorious_Deeds, Dark_Doings, Fiendish_Frolics, Mysterious_Happenings :
     Book.Handle;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Test_Case) is

      pragma Warnings (Off, T);

      Cl : Current.Handle;
      pragma Warnings (Off, cl);
      Lh : History.Handle;
      pragma Warnings (Off, Lh);

   begin

      Library.Initialize;

      Alice := Borrower.Create ((Name => +"Alice"));
      Bob := Borrower.Create ((Name => +"Bob"));
      Carol := Borrower.Create ((Name => +"Carol"));
      Dave := Borrower.Create ((Name => +"Dave"));

      Glorious_Deeds := Book.Create
        ((Title => +"Glorious Deeds"));
      Dark_Doings := Book.Create
        ((Title => +"Dark Doings"));
      Fiendish_Frolics := Book.Create
        ((Title => +"Fiendish Frolics"));
      Mysterious_Happenings := Book.Create
        ((Title => +"Mysterious Happenings"));

      Authorship.Link (Was_Written_By => Alice,
                       Wrote => Glorious_Deeds);
      Authorship.Link (Was_Written_By => Alice,
                       Wrote => Dark_Doings);
      Authorship.Link (Was_Written_By => Alice,
                       Wrote => Fiendish_Frolics);
      Authorship.Link (Was_Written_By => Bob,
                       Wrote => Mysterious_Happenings);

      Cl := Current.Link (Is_On_Loan_To => Alice,
                          Is_Borrowing => Mysterious_Happenings);

      Lh := History.Link (Has_Been_Loaned_To => Alice,
                          Has_Borrowed => Mysterious_Happenings);

      Lh := History.Link (Has_Been_Loaned_To => Bob,
                          Has_Borrowed => Glorious_Deeds);
      Lh := History.Link (Has_Been_Loaned_To => Bob,
                          Has_Borrowed => Dark_Doings);

      Lh := History.Link (Has_Been_Loaned_To => Carol,
                          Has_Borrowed => Mysterious_Happenings);
      Lh := History.Link (Has_Been_Loaned_To => Carol,
                          Has_Borrowed => Glorious_Deeds);
      Lh := History.Link (Has_Been_Loaned_To => Carol,
                          Has_Borrowed => Dark_Doings);

   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out Test_Case) is
      pragma Warnings (Off, T);
   begin

      Library.Tear_Down;

   end Tear_Down;


   -------------------
   -- Test Routines --
   -------------------


   procedure T1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      B : Borrower.Handle;
      pragma Warnings (Off, B);
   begin
      B := Authorship.Wrote (null);
   exception
      when Constraint_Error => null;
   end T1;

   procedure T2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      B : Borrower.Handle;
      use type Borrower.Handle;
   begin
      B := Authorship.Wrote (Mysterious_Happenings);
      Assert (B = Bob, "B = Bob");
   end T2;

   procedure T3 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Vectors.Vector;
      use type Ada.Containers.Count_Type;
   begin
      Bks := Authorship.Was_Written_By (null);
      Assert (Book.Vectors.Length (Bks) = 0,
              "Book.Vectors.Length (Bks) = 0");
   end T3;

   procedure T4 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Vectors.Vector;
      use type Ada.Containers.Count_Type;
   begin
      Bks := Authorship.Was_Written_By (Carol);
      Assert (Book.Vectors.Length (Bks) = 0,
              "Book.Vectors.Length (Bks) = 0");
   end T4;

   procedure T5 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Vectors.Vector;
      use type Ada.Containers.Count_Type;
   begin
      Bks := Authorship.Was_Written_By (Alice);
      Assert (Book.Vectors.Length (Bks) = 3,
              "Book.Vectors.Length (Bks) = 3");
      Assert (Book.Vectors.Contains (Bks, Glorious_Deeds),
              "Book.Vectors.Contains (Bks, Glorious_Deeds)");
      Assert (Book.Vectors.Contains (Bks, Dark_Doings),
              "Book.Vectors.Contains (Bks, Dark_Doings)");
      Assert (Book.Vectors.Contains (Bks, Fiendish_Frolics),
              "Book.Vectors.Contains (Bks, Fiendish_Frolics)");
      Assert (not Book.Vectors.Contains (Bks, Mysterious_Happenings),
              "not Book.Vectors.Contains (Bks, Mysterious_Happenings)");
   end T5;

   procedure T6 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bs : Borrower.Vectors.Vector;
      use type Ada.Containers.Count_Type;
   begin
      Bs := Authorship.From_Vectors.Wrote (Book.Vectors.Empty_Vector);
      Assert (Borrower.Vectors.Length (Bs) = 0,
              "Borrower.Vectors.Length (Bs) = 0");
   end T6;

   procedure T7 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bs : Borrower.Vectors.Vector;
      use type Ada.Containers.Count_Type;
   begin
      Bs := Authorship.From_Vectors.Wrote (Book.All_Instances);
      Assert (Borrower.Vectors.Length (Bs) = 2,
              "Borrower.Vectors.Length (Bs) = 2");
      Assert (Borrower.Vectors.Contains (Bs, Alice),
              "Borrower.Vectors.Contains (Bs, Alice)");
      Assert (Borrower.Vectors.Contains (Bs, Bob),
              "Borrower.Vectors.Contains (Bs, Bob)");
      Assert (not Borrower.Vectors.Contains (Bs, Carol),
              "Borrower.Vectors.Contains (Bs, Carol)");
      Assert (not Borrower.Vectors.Contains (Bs, Dave),
              "not Borrower.Vectors.Contains (Bs, Dave)");
   end T7;

   procedure T8 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Vectors.Vector;
      use type Ada.Containers.Count_Type;
   begin
      Bks := Authorship.From_Vectors.Was_Written_By
        (Borrower.Vectors.Empty_Vector);
      Assert (Book.Vectors.Length (Bks) = 0,
              "Book.Vectors.Length (Bks) = 0");
   end T8;

   procedure T9 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Vectors.Vector;
      use type Ada.Containers.Count_Type;
   begin
      Bks := Authorship.From_Vectors.Was_Written_By
        (Borrower.All_Instances);
      Assert (Book.Vectors.Length (Bks) = 4,
              "Book.Vectors.Length (Bks) = 4");
   end T9;

   procedure T10 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Cl : Current.Handle;
      pragma Warnings (Off, Cl);
   begin
      Cl := Current.Link (Is_On_Loan_To => Alice,
                          Is_Borrowing => Mysterious_Happenings);
   exception
      when ColdFrame.Exceptions.Duplicate => null;
   end T10;

   procedure T11 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      B : Borrower.Handle;
      use type Borrower.Handle;
   begin
      B := Current.Is_Borrowing (Book.Handle'(null));
      Assert (B = null, "B = null");
   end T11;

   procedure T12 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      B : Borrower.Handle;
      use type Borrower.Handle;
   begin
      B := Current.Is_Borrowing (Glorious_Deeds);
      Assert (B = null, "B = null");
   end T12;

   procedure T13 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      B : Borrower.Handle;
      use type Borrower.Handle;
   begin
      B := Current.Is_Borrowing (Mysterious_Happenings);
      Assert (B = Alice, "B = Alice");
   end T13;

   procedure T14 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Vectors.Vector;
      use type Ada.Containers.Count_Type;
   begin
      Bks := Current.Is_On_Loan_To (null);
      Assert (Book.Vectors.Length (Bks) = 0,
              "Book.Vectors.Length (Bks) = 0");
   end T14;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, T1'Access, "1:mc, b->a, null input handle");
      Register_Routine (T, T2'Access, "1:mc, b->a");
      Register_Routine (T, T3'Access, "1:mc, a->b, null input handle");
      Register_Routine (T, T4'Access, "1:mc, a->b, empty result");
      Register_Routine (T, T5'Access, "1:mc, a->b, multiple results");
      Register_Routine (T, T6'Access, "1:mc, vector b->a, null input");
      Register_Routine (T, T7'Access, "1:mc, vector b->a");
      Register_Routine (T, T8'Access, "1:mc, vector a->b, null input");
      Register_Routine (T, T9'Access, "1:mc, vector a->b");
      Register_Routine (T, T10'Access, "1-(1c:mc), create duplicate link");
      Register_Routine (T, T11'Access, "1-(1c:mc), b->a, null input handle");
      Register_Routine (T, T12'Access, "1-(1c:mc), b->a, null result");
      Register_Routine (T, T13'Access, "1-(1c:mc), b->a");
      Register_Routine (T, T14'Access, "1-(1c:mc), a->b, null input handle");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return AUnit.Message_String is
      pragma Warnings (Off, T);
   begin
      return new String'("Association tests");
   end Name;

end Library.Tests;
