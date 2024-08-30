--  Copyright (C) 2024 Free Software Foundation, Inc.

--  This file is part of the Cortex GNAT RTS package.
--
--  The Cortex GNAT RTS package is free software; you can redistribute
--  it and/or modify it under the terms of the GNU General Public
--  License as published by the Free Software Foundation; either
--  version 3 of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING3.  If not, see
--  <http://www.gnu.org/licenses/>.

--  This is for the RP2350 Risc-V chip in the Pico 2 board.
--
--  It would have been great to use the on-board RGB LED, but its
--  access is apparently quite complex, so we use an ordinary LED
--  accessed via GPIO0.

with Ada.Real_Time;
with HAL.GPIO;
with RP2350_HAL.GPIO; use RP2350_HAL.GPIO;

package body Heartbeat is

   task Beat
      with Storage_Size => 1024,
        Secondary_Stack_Size => 0
   is
      pragma Task_Name ("heartbeat.beat");
   end Beat;

   task body Beat is
      use type Ada.Real_Time.Time;
      LED : GPIO_Point := (Pin => 25, others => <>);
   begin
      Configure (LED, Mode => HAL.GPIO.Output);
      --  flash for 1 second at startup
      for J in 1 .. 5 loop
         LED.Set;
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
         LED.Clear;
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
      end loop;

      --  flash every second while running
      loop
         LED.Set;
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
         LED.Clear;
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (900);
      end loop;
   end Beat;

end Heartbeat;
