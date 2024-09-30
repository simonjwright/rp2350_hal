with HAL.GPIO; use HAL.GPIO;

package RP2350_HAL.GPIO is

   type QFN_80_Pin is range 0 .. 47;
   subtype GPIO_Pin is QFN_80_Pin range 0 .. 29;

   type GPIO_Point is new HAL.GPIO.GPIO_Point
      with record
         Pin          : GPIO_Pin;
         Current_Mode : GPIO_Mode := Unknown_Mode;
      end record;

   --  type GPIO_Function is
   --    (SPI, UART, I2C, PWM, SIO, PIO0, PIO1, CLOCK, USB, HI_Z);

   type GPIO_Pull_Mode is (Floating, Pull_Up, Pull_Down, Pull_Both);

   type GPIO_Drive is (Drive_2mA, Drive_4mA, Drive_8mA, Drive_12mA);

   type Interrupt_Triggers is
     (Low_Level, High_Level, Falling_Edge, Rising_Edge, Any_Edge);

   procedure Enable
   with Post => Enabled;

   function Enabled return Boolean;

   procedure Configure
     (This  : in out GPIO_Point;
      Mode  :        GPIO_Config_Mode;
      Pull  :        GPIO_Pull_Mode := Floating;
      Drive :        GPIO_Drive     := Drive_4mA)
   with Pre => Enabled;

   function Get
     (This : GPIO_Point)
     return Boolean
   with Pre => Enabled;
   --  RP2350 allows us to read a pin's state even if it's being used
   --  as an output pin.

   type Handler_Procedure is access procedure (Pin : GPIO_Pin);

   procedure Enable_Interrupt
     (This    : in out GPIO_Point;
      Trigger : Interrupt_Triggers;
      Handler : Handler_Procedure)
   with Pre => Enabled and then Handler /= null;

   procedure Disable_Interrupt
     (This    : in out GPIO_Point;
      Trigger : Interrupt_Triggers)
   with Pre => Enabled;

   procedure Acknowledge_Interrupt
     (This     : GPIO_Point;
      Trigger : Interrupt_Triggers)
   with Pre => Enabled;

   function Interrupt_Status
     (This     : GPIO_Point;
      Trigger : Interrupt_Triggers)
     return Boolean
   with Pre => Enabled;

   overriding
   function Support
     (This : GPIO_Point;
      Capa : HAL.GPIO.Capability)
     return Boolean
   with Pre => Enabled;

   overriding
   function Mode
     (This : GPIO_Point)
     return HAL.GPIO.GPIO_Mode
   with Pre => Enabled;

   overriding
   procedure Set_Mode
     (This : in out GPIO_Point;
      Mode : HAL.GPIO.GPIO_Config_Mode)
   with Pre => Enabled;

   overriding
   function Pull_Resistor
     (This : GPIO_Point)
     return HAL.GPIO.GPIO_Pull_Resistor
   with Pre => Enabled;

   overriding
   procedure Set_Pull_Resistor
     (This : in out GPIO_Point;
      Pull : HAL.GPIO.GPIO_Pull_Resistor)
   with Pre => Enabled;

   overriding
   function Set
     (This : GPIO_Point)
     return Boolean
   with Pre => Enabled;

   overriding
   procedure Set
     (This : in out GPIO_Point)
   with Pre => Enabled;

   overriding
   procedure Clear
     (This : in out GPIO_Point)
   with Pre => Enabled;

   overriding
   procedure Toggle
     (This : in out GPIO_Point)
   with Pre => Enabled;

private

end RP2350_HAL.GPIO;
