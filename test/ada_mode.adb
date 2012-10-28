package body Ada_Mode is

   protected body Separate_Protected_Body is separate;

   task Separate_Task_Body is
      entry Please_Abort;
   end;

   task body Separate_Task_Body is separate;

   procedure Separate_Procedure is separate;

   function Separate_Function return Integer is separate;

end Ada_Mode;
