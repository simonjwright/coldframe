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

with Digital_IO_Support;

package Digital_IO.STM32F4_Support is

   procedure Initialize;

private

   type Implementation
     is new Digital_IO_Support.Implementation with null record;

   overriding
   function Get (This      : Implementation;
                 For_Input : Digital_IO_Support.Input_Signal) return Boolean;

   overriding
   procedure Set (This       : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To         : Boolean);

   type IRQ_Handler is protected interface;
   procedure Button_Pressed
     (Handler : in out IRQ_Handler;
      Button  :    out Digital_IO_Support.Input_Signal)
     is abstract;

end Digital_IO.STM32F4_Support;
