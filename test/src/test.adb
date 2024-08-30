with HAL.GPIO;
with RP2350_HAL.GPIO; use RP2350_HAL.GPIO;

with HAL.Time;
with RP2350_HAL.Time;

with Heartbeat;
pragma Unreferenced (Heartbeat);

--  with Test_Handler;

procedure Test is
   --  It seems that Risc-V needs more than normal task storage size.
   Environment_Task_Storage_Size : constant Natural := 2048
     with
       Export,
       Convention => Ada,
       External_Name => "_environment_task_storage_size";

   Default_Storage_Size : constant Natural := 2048
     with
       Export,
       Convention => Ada,
       External_Name => "_default_storage_size";

   Output_Pin : GPIO_Point := (Pin => 16, others => <>);
   Input_Pin  : GPIO_Point := (Pin => 3, others => <>);

begin

   Configure (Output_Pin,
              Mode => HAL.GPIO.Output);

   Configure (Input_Pin,
              Mode => HAL.GPIO.Input,
              Pull => Pull_Up);

   loop

      if Input_Pin.Set then
         Output_Pin.Clear;
      else
         Output_Pin.Set;
      end if;

      HAL.Time.Delay_Milliseconds (RP2350_HAL.Time.Delays.all, 100);

   end loop;

end Test;
