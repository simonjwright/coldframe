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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Unchecked_Deallocation;

package body ColdFrame.Interrupts is


   procedure Attach (H : in out Handler;
                     To : Ada.Interrupts.Interrupt_ID) is
   begin
      Ada.Interrupts.Exchange_Handler
        (Old_Handler => H.Old_Handler,
         New_Handler => H.Handling_PO.Trigger'Access,
         Interrupt => To);
      H.Attached_Interrupt := To;
   end Attach;


   procedure Wait (On : Handler) is
   begin
      On.Handling_PO.Wait;
   end Wait;


   protected body Handling is

      entry Wait when Ready is
      begin
         Ready := False;
      end Wait;

      procedure Trigger is
      begin
         Ready := True;
      end Trigger;

   end Handling;


   procedure Initialize (H : in out Handler) is
   begin
      H.Handling_PO := new Handling;
   end Initialize;


   procedure Finalize (H : in out Handler) is
      procedure Free is new Ada.Unchecked_Deallocation (Handling, Handling_P);
      use type Ada.Interrupts.Parameterless_Handler;
   begin
      if H.Old_Handler /= null then
         Ada.Interrupts.Attach_Handler (New_Handler => H.Old_Handler,
                                        Interrupt => H.Attached_Interrupt);
      else
         Ada.Interrupts.Detach_Handler (Interrupt => H.Attached_Interrupt);
      end if;
      Free (H.Handling_PO);
   end Finalize;


end ColdFrame.Interrupts;
