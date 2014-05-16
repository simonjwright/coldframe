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

with Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with ColdFrame.Instances;
with Van_Fleet.Customer;
with Van_Fleet.Initialize;
with Van_Fleet.Pool_Van.Inheritance;
with Van_Fleet.Van.All_Instances;
with Van_Fleet.Van.Vectors;

procedure Van_Fleet.Demo is

   procedure Print (V : Van.Vectors.Cursor);
   procedure Print (V : Van.Vectors.Cursor) is
   begin
      Put_Line (Van.Image (Van.Vectors.Element (V)));
   end Print;

begin

   Initialize;

   declare
      CH : Customer.Handle;
      pragma Warnings (Off, CH);
   begin
      CH := Customer.Create
        ((Name => To_Unbounded_String ("Alice")));
      CH := Customer.Create
        ((Name => To_Unbounded_String ("Bob")));
      CH := Customer.Create
        ((Name => To_Unbounded_String ("Carol")));
      CH := Customer.Create
        ((Name => To_Unbounded_String ("Dave")));
   end;

   declare
      VH : Van.Handle;
      PVH : Pool_Van.Handle;
      pragma Warnings (Off, PVH);
   begin
      VH := Van.Create
        ((Index => To_Unbounded_String ("APR81")));
      PVH := Pool_Van.Inheritance.Create_Tree
        (ColdFrame.Instances.Handle (VH));
      VH := Van.Create
        ((Index => To_Unbounded_String ("8493KC")));
      PVH := Pool_Van.Inheritance.Create_Tree
        (ColdFrame.Instances.Handle (VH));
      VH := Van.Create
        ((Index => To_Unbounded_String ("NHO864F")));
      PVH := Pool_Van.Inheritance.Create_Tree
        (ColdFrame.Instances.Handle (VH));
      VH := Van.Create
        ((Index => To_Unbounded_String ("PBL196R")));
      PVH := Pool_Van.Inheritance.Create_Tree
        (ColdFrame.Instances.Handle (VH));
   end;

   declare
      VH : Van.Handle;
      pragma Warnings (Off, VH);
      use type Ada.Calendar.Time;
   begin
      VH := Van.Lend
        (To => ColdFrame.Instances.Handle
           (Customer.Find
              ((Name => To_Unbounded_String ("Alice")))),
         Terminating_At => Ada.Calendar.Clock + 86400.0);
      VH := Van.Lend
        (To => ColdFrame.Instances.Handle
           (Customer.Find
              ((Name => To_Unbounded_String ("Alice")))),
         Terminating_At => Ada.Calendar.Clock + 86400.0);
      VH := Van.Lend
        (To => ColdFrame.Instances.Handle
           (Customer.Find
              ((Name => To_Unbounded_String ("Carol")))),
         Terminating_At => Ada.Calendar.Clock + 86400.0);
      VH := Van.Lend
        (To => ColdFrame.Instances.Handle
           (Customer.Find
              ((Name => To_Unbounded_String ("Dave")))),
         Terminating_At => Ada.Calendar.Clock + 86400.0);
   end;

   declare
      VH : Van.Handle;
      pragma Warnings (Off, VH);
      use type Ada.Calendar.Time;
   begin
      VH := Van.Lend
        (To => ColdFrame.Instances.Handle
           (Customer.Find
              ((Name => To_Unbounded_String ("Bob")))),
         Terminating_At => Ada.Calendar.Clock + 86400.0);
      Put_Line ("shouldn't have managed to lend a van to Bob.");
   exception
      when Not_Found => null;
   end;

   Put_Line ("printing all instances");
   Van.All_Instances.Iterate (Print'Access);

   New_Line;
   Put_Line ("returning PBL196R");
   Van.Returned (Van.Find ((Index => To_Unbounded_String ("PBL196R"))));

   New_Line;
   Put_Line ("printing all instances");
   Van.All_Instances.Iterate (Print'Access);

   New_Line;
   Put_Line ("printing all instances (sorted)");
   declare
      function "<" (L, R : Van.Handle) return Boolean;
      package Sorting is new Van.Vectors.Generic_Sorting ("<" => "<");
      function "<" (L, R : Van.Handle) return Boolean is
      begin
         return Van.Image (L) < Van.Image (R);
      end "<";
      Instances : Van.Vectors.Vector := Van.All_Instances;
   begin
      Sorting.Sort (Instances);
      Instances.Iterate (Print'Access);
   end;

end Van_Fleet.Demo;
