with Ada.Real_Time; use Ada.Real_Time;

package body RP2350_HAL.Time is

   Delay_Singleton : aliased Ravenscar_Delays;

   ------------
   -- Delays --
   ------------

   function Delays return not null HAL.Time.Any_Delays is
   begin
      return Delay_Singleton'Access;
   end Delays;

   ------------------------
   -- Delay_Microseconds --
   ------------------------

   overriding
   procedure Delay_Microseconds
     (This : in out Ravenscar_Delays;
      Us   : Integer)
   is
      pragma Unreferenced (This);
   begin
      delay until Clock + Microseconds (Us);
   end Delay_Microseconds;

   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   overriding
   procedure Delay_Milliseconds
     (This : in out Ravenscar_Delays;
      Ms   : Integer)
   is
      pragma Unreferenced (This);
   begin
      delay until Clock + Milliseconds (Ms);
   end Delay_Milliseconds;

   -------------------
   -- Delay_Seconds --
   -------------------

   overriding
   procedure Delay_Seconds
     (This : in out Ravenscar_Delays;
      S    : Integer)
   is
      pragma Unreferenced (This);
   begin
      delay until Clock + Seconds (S);
   end Delay_Seconds;

end RP2350_HAL.Time;
