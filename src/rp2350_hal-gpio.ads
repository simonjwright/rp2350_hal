with HAL.GPIO; use HAL.GPIO;

package RP2350_HAL.GPIO is

   type GPIO_Pin is range 0 .. 47;

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

   procedure Enable;

   function Enabled return Boolean;

   procedure Configure
     (This  : in out GPIO_Point;
      Mode  :        GPIO_Config_Mode;
      Pull  :        GPIO_Pull_Mode := Floating;
      Drive :        GPIO_Drive     := Drive_4mA);

   function Get
     (This : GPIO_Point)
     return Boolean;
   --  RP2350 allows us to read a pin's state even if it's being used
   --  as an output pin.

   procedure Enable_Interrupt
     (This    : in out GPIO_Point;
      Trigger : Interrupt_Triggers);

   procedure Disable_Interrupt
     (This    : in out GPIO_Point;
      Trigger : Interrupt_Triggers);

   procedure Acknowledge_Interrupt
     (This     : GPIO_Point;
      Trigger : Interrupt_Triggers);

   function Interrupt_Status
     (This     : GPIO_Point;
      Trigger : Interrupt_Triggers)
     return Boolean;

   overriding
   function Support
     (This : GPIO_Point;
      Capa : HAL.GPIO.Capability)
     return Boolean;

   overriding
   function Mode
     (This : GPIO_Point)
     return HAL.GPIO.GPIO_Mode;

   overriding
   procedure Set_Mode
     (This : in out GPIO_Point;
      Mode : HAL.GPIO.GPIO_Config_Mode);

   overriding
   function Pull_Resistor
     (This : GPIO_Point)
     return HAL.GPIO.GPIO_Pull_Resistor;

   overriding
   procedure Set_Pull_Resistor
     (This : in out GPIO_Point;
      Pull : HAL.GPIO.GPIO_Pull_Resistor);

   overriding
   function Set
     (This : GPIO_Point)
     return Boolean;

   overriding
   procedure Set
     (This : in out GPIO_Point);

   overriding
   procedure Clear
     (This : in out GPIO_Point);

   overriding
   procedure Toggle
     (This : in out GPIO_Point);

private

end RP2350_HAL.GPIO;
