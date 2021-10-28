--  From Simon Write

with Ada.Real_Time.Timing_Events;
package Pragma_In_Po is

   protected type Flasher_Handler is
      pragma Interrupt_Priority;
      procedure Turn_Off_The_Led
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event);
   end Flasher_Handler;

end Pragma_In_Po;
