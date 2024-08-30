--  This package is based on
--  Ada_Drivers_Library//middleware/src/ravenscar-common/ravenscar_time.

with HAL.Time;

package RP2350_HAL.Time is

   function Delays return not null HAL.Time.Any_Delays;

private

   type Ravenscar_Delays is new HAL.Time.Delays with null record;

   overriding
   procedure Delay_Microseconds (This : in out Ravenscar_Delays;
                                 Us   : Integer);

   overriding
   procedure Delay_Milliseconds (This : in out Ravenscar_Delays;
                                 Ms   : Integer);

   overriding
   procedure Delay_Seconds      (This : in out Ravenscar_Delays;
                                 S    : Integer);
end RP2350_HAL.Time;
