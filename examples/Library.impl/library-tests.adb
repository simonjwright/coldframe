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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ColdFrame.Exceptions;

with Library.Initialize;
with Library.Tear_Down;

with Library.Book.All_Instances;
with Library.Book.Collections;
with Library.Borrower.All_Instances;
with Library.Borrower.Collections;
with Library.Current;
with Library.Current_Class;
with Library.History;
with Library.History_Class;

with Library.Authorship.From_Collections;

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

      Cl : Current_Class.Handle;
      pragma Warnings (Off, cl);
      Lh : History_Class.Handle;
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
      use type Borrower.Handle;
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
      Assert (T, B = Bob, "B = Bob");
   end T2;

   procedure T3 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Collections.Collection;
      use type Book.Handle;
   begin
      Bks := Authorship.Was_Written_By (null);
      Assert (T,
              Book.Collections.Length (Bks) = 0,
              "Book.Collections.Length (Bks) = 0");
   end T3;

   procedure T4 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Collections.Collection;
      use type Book.Handle;
   begin
      Bks := Authorship.Was_Written_By (Carol);
      Assert (T,
              Book.Collections.Length (Bks) = 0,
              "Book.Collections.Length (Bks) = 0");
   end T4;

   procedure T5 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Collections.Collection;
      use type Book.Handle;
   begin
      Bks := Authorship.Was_Written_By (Alice);
      Assert (T,
              Book.Collections.Length (Bks) = 3,
              "Book.Collections.Length (Bks) = 3");
      Assert (T,
              Book.Collections.Location (Bks, Glorious_Deeds) /= 0,
              "Book.Collections.Location (Bks, Glorious_Deeds) /= 0");
      Assert (T,
              Book.Collections.Location (Bks, Dark_Doings) /= 0,
              "Book.Collections.Location (Bks, Dark_Doings) /= 0");
      Assert (T,
              Book.Collections.Location (Bks, Fiendish_Frolics) /= 0,
              "Book.Collections.Location (Bks, Fiendish_Frolics) /= 0");
      Assert (T,
              Book.Collections.Location (Bks, Mysterious_Happenings) = 0,
              "Book.Collections.Location (Bks, Mysterious_Happenings) = 0");
   end T5;

   procedure T6 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bs : Borrower.Collections.Collection;
   begin
      Bs := Authorship.From_Collections.Wrote (Book.Collections.Null_Container);
      Assert (T,
              Borrower.Collections.Length (Bs) = 0,
              "Borrower.Collections.Length (Bs) = 0");
   end T6;

   procedure T7 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bs : Borrower.Collections.Collection;
   begin
      Bs := Authorship.From_Collections.Wrote (Book.All_Instances);
      Assert (T,
              Borrower.Collections.Length (Bs) = 2,
              "Borrower.Collections.Length (Bs) = 2");
      Assert (T,
              Borrower.Collections.Location (Bs, Alice) /= 0,
              "Borrower.Collections.Location (Bs, Alice) /= 0");
      Assert (T,
              Borrower.Collections.Location (Bs, Bob) /= 0,
              "Borrower.Collections.Location (Bs, Bob) /= 0");
      Assert (T,
              Borrower.Collections.Location (Bs, Carol) = 0,
              "Borrower.Collections.Location (Bs, Carol) = 0");
      Assert (T,
              Borrower.Collections.Location (Bs, Dave) = 0,
              "Borrower.Collections.Location (Bs, Dave) = 0");
   end T7;

   procedure T8 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Collections.Collection;
   begin
      Bks := Authorship.From_Collections.Was_Written_By
        (Borrower.Collections.Null_Container);
      Assert (T,
              Book.Collections.Length (Bks) = 0,
              "Book.Collections.Length (Bks) = 0");
   end T8;

   procedure T9 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Collections.Collection;
   begin
      Bks := Authorship.From_Collections.Was_Written_By
        (Borrower.All_Instances);
      Assert (T,
              Book.Collections.Length (Bks) = 4,
              "Book.Collections.Length (Bks) = 4");
   end T9;

   procedure T10 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Cl : Current_Class.Handle;
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
      Assert (T, B = null, "B = null");
   end T11;

   procedure T12 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      B : Borrower.Handle;
      use type Borrower.Handle;
   begin
      B := Current.Is_Borrowing (Glorious_Deeds);
      Assert (T, B = null, "B = null");
   end T12;

   procedure T13 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      B : Borrower.Handle;
      use type Borrower.Handle;
   begin
      B := Current.Is_Borrowing (Mysterious_Happenings);
      Assert (T, B = Alice, "B = Alice");
   end T13;

   procedure T14 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, T);
      Bks : Book.Collections.Collection;
   begin
      Bks := Current.Is_On_Loan_To (null);
      Assert (T,
              Book.Collections.Length (Bks) = 0,
              "Book.Collections.Length (Bks) = 0");
   end T14;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, T1'Access, "1:mc, b->a, null input handle");
      Register_Routine (T, T2'Access, "1:mc, b->a");
      Register_Routine (T, T3'Access, "1:mc, a->b, null input handle");
      Register_Routine (T, T4'Access, "1:mc, a->b, empty result");
      Register_Routine (T, T5'Access, "1:mc, a->b, multiple results");
      Register_Routine (T, T6'Access, "1:mc, collection b->a, null input");
      Register_Routine (T, T7'Access, "1:mc, collection b->a");
      Register_Routine (T, T8'Access, "1:mc, collection a->b, null input");
      Register_Routine (T, T9'Access, "1:mc, collection a->b");
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
