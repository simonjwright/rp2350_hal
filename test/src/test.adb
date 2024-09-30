with HAL.GPIO;
with RP2350_HAL.GPIO; use RP2350_HAL.GPIO;

with HAL.Time;
with RP2350_HAL.Time;

with Heartbeat;
pragma Unreferenced (Heartbeat);

procedure Test is
   --  It seems that Risc-V needs more than normal task storage size.

   Default_Initial_Stack : constant Natural := 4096
     with
       Export,
       Convention => Ada,
       External_Name => "_default_initial_stack";

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

   procedure Receive_Notification (For_Pin : RP2350_HAL.GPIO.GPIO_Pin);

   procedure Receive_Notification (For_Pin : RP2350_HAL.GPIO.GPIO_Pin)
   is
   begin
      pragma Assert (For_Pin = Input_Pin.Pin);
      if Input_Pin.Set then
         Output_Pin.Clear;
      else
         Output_Pin.Set;
      end if;
   end Receive_Notification;

begin

   Enable;

   Configure (Output_Pin,
              Mode => HAL.GPIO.Output);

   Configure (Input_Pin,
              Mode => HAL.GPIO.Input,
              Pull => Pull_Up);

   Input_Pin.Enable_Interrupt
     (Trigger => Any_Edge,
      Handler => Receive_Notification'Unrestricted_Access);

   loop
      HAL.Time.Delay_Milliseconds (RP2350_HAL.Time.Delays.all, 25);
   end loop;

end Test;
