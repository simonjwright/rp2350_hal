pragma Ada_2022;

with Ada.Interrupts.Names;
with Interfaces;
with RP2350.RESETS; use RP2350.RESETS;
with RP2350.IO_BANK0; use RP2350.IO_BANK0;
with RP2350.PADS_BANK0; use RP2350.PADS_BANK0;
with RP2350.SIO; use RP2350.SIO;

with System;

with RP2350.IO_BANK0.Patched; use RP2350.IO_BANK0.Patched;

package body RP2350_HAL.GPIO is

   use RP2350;

   function "not" (L : RP2350.Bit) return RP2350.Bit
     is (if L = 0 then 1 else 0);

   function "and" (L, R : RP2350.Bit) return RP2350.Bit
     is (if L = 1 and then R = 1 then 1 else 0);

   function "or" (L, R : RP2350.Bit) return RP2350.Bit
     is (if L = 1 or  R = 1 then 1 else 0);

   function "not" (L : INTERRUPT_INFO_FIELD) return INTERRUPT_INFO_FIELD
     is ((Level_Low  => not L.Level_Low,
          Level_High => not L.Level_High,
          Edge_Low   => not L.Edge_Low,
          Edge_High  => not L.Edge_High));

   function "and" (L, R : INTERRUPT_INFO_FIELD) return INTERRUPT_INFO_FIELD
     is ((Level_Low  => L.Level_Low and R.Level_Low,
          Level_High => L.Level_High and R.Level_High,
          Edge_Low   => L.Edge_Low and R.Edge_Low,
          Edge_High  => L.Edge_High and R.Edge_High));

   function "or" (L, R : INTERRUPT_INFO_FIELD) return INTERRUPT_INFO_FIELD
     is ((Level_Low  => L.Level_Low or R.Level_Low,
          Level_High => L.Level_High or R.Level_High,
          Edge_Low   => L.Edge_Low or R.Edge_Low,
          Edge_High  => L.Edge_High or R.Edge_High));

   function Make_Triggers (Trigger : Interrupt_Triggers)
                          return INTERRUPT_INFO_FIELD
     is ((Level_Low  => (case Trigger is
                          when Low_Level => 1,
                          when others    => 0),
          Level_High => (case Trigger is
                          when High_Level => 1,
                          when others => 0),
          Edge_Low   => (case Trigger is
                          when Falling_Edge | Any_Edge => 1,
                          when others                  => 0),
          Edge_High  => (case Trigger is
                          when Rising_Edge | Any_Edge => 1,
                          when others                 => 0)));

   Unimplemented : exception;

   Handlers : array (QFN_80_Pin) of Handler_Procedure;

   protected Interrupt_Handler
   with Interrupt_Priority => System.Interrupt_Priority'First + 1
   is
      entry Wait;
   private
      Triggered : Boolean := False;
      procedure Handler
      with Attach_Handler => Ada.Interrupts.Names.IO_IRQ_BANK0_Interrupt;
   end Interrupt_Handler;

   GPIO_Enabled : Boolean := False;

   procedure Enable
   is
   begin
      --  This needs to be idempotent.
      if not GPIO_Enabled then
         IO_BANK0_Periph.PROC0_INTE := (others => (others => 0));
         IO_BANK0_Periph.INTR := (others => (others => 1)); -- bits are WC

         GPIO_Enabled := True;
      end if;
   end Enable;

   function Enabled return Boolean is (GPIO_Enabled);

   procedure Configure
     (This  : in out GPIO_Point;
      Mode  :        GPIO_Config_Mode;
      Pull  :        GPIO_Pull_Mode := Floating;
      Drive :        GPIO_Drive     := Drive_4mA)
   is
      use RP2350;
      Pin_Bit : constant UInt32 := Shift_Left (1, Natural (This.Pin));
   begin
      --  The pin needs to have the SIO function, at any rate for
      --  output.
      IO_BANK0_Periph.GPIO (Natural (This.Pin)).GPIO_CTRL.FUNCSEL
        := IO_BANK0.sio;

      case Mode is
         when Input =>
            This.Current_Mode := Input;

            SIO_Periph.GPIO_OE_CLR := Pin_Bit;

            PADS_BANK0_Periph.GPIO (Natural (This.Pin)).IE := 1;

            case Pull is
               when Floating =>
                  PADS_BANK0_Periph.GPIO (Natural (This.Pin)).PUE := 0;
                  PADS_BANK0_Periph.GPIO (Natural (This.Pin)).PDE := 0;
               when Pull_Up =>
                  PADS_BANK0_Periph.GPIO (Natural (This.Pin)).PUE := 1;
                  PADS_BANK0_Periph.GPIO (Natural (This.Pin)).PDE := 0;
               when Pull_Down =>
                  PADS_BANK0_Periph.GPIO (Natural (This.Pin)).PUE := 0;
                  PADS_BANK0_Periph.GPIO (Natural (This.Pin)).PDE := 1;
               when Pull_Both =>
                  PADS_BANK0_Periph.GPIO (Natural (This.Pin)).PUE := 1;
                  PADS_BANK0_Periph.GPIO (Natural (This.Pin)).PDE := 1;
            end case;

         when Output =>
            This.Current_Mode := Output;

            SIO_Periph.GPIO_OE_SET := Pin_Bit;

            --  Don't disable output.
            PADS_BANK0_Periph.GPIO (Natural (This.Pin)).OD := 0;

            --  Set the drive strength.
            PADS_BANK0_Periph.GPIO (Natural (This.Pin)).DRIVE :=
              (case Drive is
                  when Drive_2mA  => Val_2mA,
                  when Drive_4mA  => Val_4mA,
                  when Drive_8mA  => Val_8mA,
                  when Drive_12mA => Val_12mA);

      end case;

      --  Clear isolation.
      PADS_BANK0_Periph.GPIO (Natural (This.Pin)).ISO := 0;
   end Configure;

   function Get
     (This : GPIO_Point)
     return Boolean
   is
      use RP2350;
      Pin_Bit : constant UInt32 := Shift_Left (1, Natural (This.Pin));
   begin
      return (SIO_Periph.GPIO_IN and Pin_Bit) /= 0;
   end Get;

   procedure Enable_Interrupt
     (This    : in out GPIO_Point;
      Trigger : Interrupt_Triggers;
      Handler : Handler_Procedure)
   is
      Pin : constant Natural := Natural (This.Pin);
      Info_Index : constant Natural := Pin / 8;
      Field_Index : constant Natural := Pin mod 8;
      Triggers : constant INTERRUPT_INFO_FIELD := Make_Triggers (Trigger);
      Info : INTERRUPT_INFO;
   begin
      --  Clear interrupts.
      --  The Edge bits are WC, write 1 to clear.
      Info (Field_Index) :=
        (Edge_Low => 1, Edge_High => 1, others => 0);
      INTR (Info_Index) := Info;

      --  Enable interrupts.
      Info := PROC0_INTE (Info_Index);
      Info (Field_Index) := Triggers;
      PROC0_INTE (Info_Index) := Info;

      --  XXX should check it's not already registered
      Handlers (This.Pin) := Handler;
   end Enable_Interrupt;

   procedure Disable_Interrupt
     (This    : in out GPIO_Point;
      Trigger : Interrupt_Triggers)
   is
      Pin : constant Natural := Natural (This.Pin);
      Info_Index : constant Natural := Pin / 8;
      Field_Index : constant Natural := Pin mod 8;
      Triggers : constant INTERRUPT_INFO_FIELD := Make_Triggers (Trigger);
      Info : INTERRUPT_INFO;
   begin
      Info := PROC0_INTE (Info_Index);
      Info (Field_Index) := @ and not Triggers;
      PROC0_INTE (Info_Index) := Info;
   end Disable_Interrupt;

   procedure Acknowledge_Interrupt
     (This     : GPIO_Point;
      Trigger : Interrupt_Triggers)
   is
      Pin : constant Natural := Natural (This.Pin);
      Info_Index : constant Natural := Pin / 8;
      Field_Index : constant Natural := Pin mod 8;
      Triggers : constant INTERRUPT_INFO_FIELD := Make_Triggers (Trigger);
      Info : INTERRUPT_INFO;
   begin
      raise Unimplemented;
   end Acknowledge_Interrupt;

   function Interrupt_Status
     (This     : GPIO_Point;
      Trigger : Interrupt_Triggers)
     return Boolean
   is
      Pin : constant Natural := Natural (This.Pin);
      Info_Index : constant Natural := Pin / 8;
      Field_Index : constant Natural := Pin mod 8;
      Triggers : constant INTERRUPT_INFO_FIELD := Make_Triggers (Trigger);
      Info : INTERRUPT_INFO;
   begin
      Info := INTR (Info_Index);
      return raise Unimplemented;
   end Interrupt_Status;

   overriding
   function Support
     (This : GPIO_Point;
      Capa : HAL.GPIO.Capability)
     return Boolean
   is
   begin
      return raise Unimplemented;
   end Support;

   overriding
   function Mode
      (This : GPIO_Point)
      return HAL.GPIO.GPIO_Mode
   is
   begin
      return This.Current_Mode;
   end Mode;

   overriding
   procedure Set_Mode
     (This : in out GPIO_Point;
      Mode : HAL.GPIO.GPIO_Config_Mode)
   is
   begin
      raise Unimplemented;
   end Set_Mode;

   overriding
   function Pull_Resistor
     (This : GPIO_Point)
     return HAL.GPIO.GPIO_Pull_Resistor
   is
   begin
      return raise Unimplemented;
   end Pull_Resistor;

   overriding
   procedure Set_Pull_Resistor
      (This : in out GPIO_Point;
      Pull : HAL.GPIO.GPIO_Pull_Resistor)
   is
   begin
      raise Unimplemented;
   end Set_Pull_Resistor;

   overriding
   function Set
     (This : GPIO_Point)
     return Boolean
   is
      use RP2350;
      Pin_Bit : constant UInt32 := Shift_Left (1, Natural (This.Pin));
   begin
      return (SIO_Periph.GPIO_IN and Pin_Bit) /= 0;
   end Set;

   overriding
   procedure Set
     (This : in out GPIO_Point)
   is
      use RP2350;
      Pin_Bit : constant UInt32 := Shift_Left (1, Natural (This.Pin));
   begin
      SIO_Periph.GPIO_OUT_SET := Pin_Bit;
      pragma Assert ((SIO_Periph.GPIO_OUT and  Pin_Bit) /= 0,
                     "wtf");
      --  pragma Assert (This.Get, "pin" & This.Pin'Image & " wasn't set");
   end Set;

   overriding
   procedure Clear
     (This : in out GPIO_Point)
   is
      use RP2350;
      Pin_Bit : constant UInt32 := Shift_Left (1, Natural (This.Pin));
   begin
      SIO_Periph.GPIO_OUT_CLR := Pin_Bit;
      pragma Assert (not This.Get, "pin" & This.Pin'Image & " wasn't cleared");
   end Clear;

   overriding
   procedure Toggle
     (This : in out GPIO_Point)
   is
      use RP2350;
      Pin_Bit : constant UInt32 := Shift_Left (1, Natural (This.Pin));
   begin
      SIO_Periph.GPIO_OUT_XOR := Pin_Bit;
   end Toggle;

   protected body Interrupt_Handler is

      entry Wait when Triggered is
      begin
         Triggered := False;
      end Wait;

      procedure Handler is
         Interrupting_Pins :
           constant IRQSUMMARY_PROC0_SECURE0_GPIO_Field_Array
           := IO_BANK0_Periph.IRQSUMMARY_PROC0_SECURE0.Arr;
      begin
         Over_GPIO_Pins :
         for P in GPIO_Pin'Range loop
            if Interrupting_Pins (Integer (P)) /= 0 then

               Clear_The_Interrupt :
               declare
                  Pin : constant Natural := Natural (P);
                  Info_Index : constant Natural := Pin / 8;
                  Field_Index : constant Natural := Pin mod 8;
                  Info : INTERRUPT_INFO;
               begin
                  --  Clear the Edge triggers (users should know what
                  --  to expect).
                  --  The Edge bits are WC, write 1 to clear.
                  Info (Field_Index) :=
                    (Edge_Low => 1, Edge_High => 1, others => 0);
                  INTR (Info_Index) := Info;
               end Clear_The_Interrupt;

               --  Call the handler if there is one.
               if Handlers (P) /= null then
                  Handlers (P) (P);
               end if;

            end if;
         end loop Over_GPIO_Pins;

         Triggered := True;
      end Handler;

   end Interrupt_Handler;

begin
   RESETS_Periph.RESET.IO_BANK0 := 1;
   RESETS_Periph.RESET.IO_BANK0 := 0;
   while RESETS_Periph.RESET_DONE.IO_BANK0 = 0 loop
      null;
   end loop;

   RESETS_Periph.RESET.PADS_BANK0 := 1;
   RESETS_Periph.RESET.PADS_BANK0 := 0;
   while RESETS_Periph.RESET_DONE.PADS_BANK0 = 0 loop
      null;
   end loop;
end RP2350_HAL.GPIO;
