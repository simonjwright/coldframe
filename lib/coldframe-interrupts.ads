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

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Ada.Finalization;
with Ada.Interrupts;

package ColdFrame.Interrupts is

   type Handler is limited private;

   procedure Attach (H : in out Handler; To : Ada.Interrupts.Interrupt_ID);

   procedure Wait (On : Handler);
   --  Blocks until the attached interrupt occurs.

private

   protected type Handling is
      entry Wait;
      procedure Trigger;
      pragma Interrupt_Handler (Trigger);
   private
      Ready : Boolean := False;
   end Handling;
   type Handling_P is access Handling;

   type Handler is new Ada.Finalization.Limited_Controlled with record
      Handling_PO : Handling_P;
      Attached_Interrupt : Ada.Interrupts.Interrupt_ID;
      Old_Handler : Ada.Interrupts.Parameterless_Handler;
   end record;

   procedure Initialize (H : in out Handler);
   procedure Finalize (H : in out Handler);

end ColdFrame.Interrupts;
