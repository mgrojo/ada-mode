package Ada_Mode.Nested_Packages is

   Local_Exception_1 : exception renames
     Global_Exception_1;

   package Sequencer is
      function Create (Model   : in Integer;
                       Context : in String) return String;
   end Sequencer;

   package Wem is
      procedure GetVariableValue;
   end Wem;

   procedure F;

   procedure Server_Begin;

   package Test_Format is
      procedure Test_Proc;
   end Test_Format;

   package TestForWhile is
      procedure Test;
   end TestForWhile;

private -- part of Ada_Mode.Nested_Packages, not Private_Only

   package Private_Only is
   private
      package Private_Package is
      end Private_Package;
      A : Integer;
   end Private_Only;

   procedure Test_For_1;

end Ada_Mode.Nested_Packages;
