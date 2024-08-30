pragma Ada_2022;

with RP2350.IO_BANK0; use RP2350.IO_BANK0;
with RP2350.PADS_BANK0; use RP2350.PADS_BANK0;
with RP2350.ACCESSCTRL; use RP2350.ACCESSCTRL;
with RP2350.SIO; use RP2350.SIO;

package body RP2350_HAL.GPIO is

   Unimplemented : exception;

   procedure Enable
   is null;

   function Enabled return Boolean is (True);

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
      --  The pin needs to be accessible to non-secure code. XXX needed?
      ACCESSCTRL_Periph.GPIO_NSMASK0 := @ or Pin_Bit;

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
      Trigger : Interrupt_Triggers)
   is
   begin
      raise Unimplemented;
      --  GPIO_Periph.PIN (Natural (This.Pin)).INT_TYPE :=
      --    (case Trigger is
      --        when Low_Level    => 4,
      --        when High_Level   => 5,
      --        when Falling_Edge => 2,
      --        when Rising_Edge  => 1,
      --        when Any_Edge     => 3);
      --  --  This is a plain CPU interrupt; 2 would be a non-maskable CPU
      --  --  interrupt (TRM Register 6.13).
      --  GPIO_Periph.PIN (Natural (This.Pin)).INT_ENA := 1;
   end Enable_Interrupt;

   procedure Disable_Interrupt
     (This    : in out GPIO_Point;
      Trigger : Interrupt_Triggers)
        is
   begin
      raise Unimplemented;
      --  GPIO_Periph.PIN (Natural (This.Pin)).INT_TYPE := 0;
      --  GPIO_Periph.PIN (Natural (This.Pin)).INT_ENA := 0;
   end Disable_Interrupt;

   procedure Acknowledge_Interrupt
     (This     : GPIO_Point;
      Trigger : Interrupt_Triggers)
   is
      use RP2350;
      Pin_Bit : constant UInt32 := Shift_Left (1, Natural (This.Pin));
   begin
      raise Unimplemented;
      --  GPIO_Periph.STATUS_W1TC := Pin_Bit;
   end Acknowledge_Interrupt;

   function Interrupt_Status
     (This     : GPIO_Point;
      Trigger : Interrupt_Triggers)
     return Boolean
   is
      use RP2350;
      Pin_Bit : constant UInt32 := Shift_Left (1, Natural (This.Pin));
   begin
      return raise Unimplemented;
      --  return (GPIO_Periph.STATUS and Pin_Bit) /= 0;
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

end RP2350_HAL.GPIO;
