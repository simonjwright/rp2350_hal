package RP2350.IO_BANK0.Patched is

   --  Interrupt Enable for proc0.

   type INTERRUPT_INFO_FIELD is record
      Level_Low  : RP2350.Bit := 0;
      Level_High : RP2350.Bit := 0;
      Edge_Low   : RP2350.Bit := 0;
      Edge_High  : RP2350.Bit := 0;
   end record with Pack, Size => 4;
   for INTERRUPT_INFO_FIELD use record
      Level_Low  at 0 range 0 .. 0;
      Level_High at 0 range 1 .. 1;
      Edge_Low   at 0 range 2 .. 2;
      Edge_High  at 0 range 3 .. 3;
   end record;

   type INTERRUPT_INFO is array (0 .. 7) of INTERRUPT_INFO_FIELD
   with Pack, Object_Size => 32;
   pragma Assert (INTERRUPT_INFO'Object_Size
                    = INTR_Register'Object_Size);
   --  There are 8 Fields in a 32-bit word.
   --  **** Can't use Volatile_Full_Access, PR116551 ****
   --  Workround: copy the INTERRUPT_INFO, modify it, write it back.

   type INTERRUPT_REGISTERS is array (0 .. 5) of INTERRUPT_INFO
   with Pack;

   --  Interrupt status for proc 0
   INTR : aliased INTERRUPT_REGISTERS
     with
       Volatile,
       Import,
       Convention => Ada,
       Address => IO_BANK0_Periph.INTR'Address;

   --  Interrupt enable for proc 0
   PROC0_INTE : aliased INTERRUPT_REGISTERS
     with
       Volatile,
       Import,
       Convention => Ada,
       Address => IO_BANK0_Periph.PROC0_INTE'Address;

end RP2350.IO_BANK0.Patched;
