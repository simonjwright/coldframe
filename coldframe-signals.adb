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

--  $RCSfile: coldframe-signals.adb,v $
--  $Revision: 23f14b3a7737 $
--  $Date: 2003/11/09 12:40:22 $
--  $Author: simon $

with Ada.Finalization;
with Ada.Synchronous_Task_Control;
with Ada.Text_IO; use Ada.Text_IO;

package body ColdFrame.Signals is


   type Signal_Handler is access procedure (S : Signal);
   pragma Convention (C, Signal_Handler);


   type Handler is new Ada.Finalization.Limited_Controlled with record
      Handling_SO : Ada.Synchronous_Task_Control.Suspension_Object;
      Attached_Signal : Signal;
      Old_Handler : Signal_Handler;
   end record;

   procedure Finalize (H : in out Handler);


   function C_Signal (S : Signal; H : Signal_Handler) return Signal_Handler;
   pragma Import (C, C_Signal, "signal");


   procedure Finalize (H : in out Handler) is
   begin
      if H.Old_Handler /= null then
         H.Old_Handler := C_Signal (H.Attached_Signal, H.Old_Handler);
      end if;
   end Finalize;


   package body Handling is


      H : Handler;


      procedure C_Handler (S : Signal);
      pragma Convention (C, C_Handler);
      procedure C_Handler (S : Signal) is
      begin
         Put_Line ("hi");
         Ada.Synchronous_Task_Control.Set_True (H.Handling_SO);
      end C_Handler;


      procedure Wait is
      begin
         Ada.Synchronous_Task_Control.Suspend_Until_True (H.Handling_SO);
      end Wait;


   begin

      H.Attached_Signal := For_Signal;
      Put_Line ("attaching");
      H.Old_Handler := C_Signal (For_Signal, C_Handler'Unrestricted_Access);
      Put_Line ("done");

   end Handling;


end ColdFrame.Signals;
