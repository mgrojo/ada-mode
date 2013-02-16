--EMACSCMD:(setq skip-recase-test t)
package Adacore_8529_012_Aux is
   type Word_Byte_Array_Unconstrained is new Integer;

   package Word_Byte_Array_Unconstrained_Text_IO is
      Default_Single_Line_Array         : constant Boolean := True;
      Default_Named_Association_Array   : constant Boolean := True;
      Default_Single_Line_Element       : constant Boolean := True;
      Default_Named_Association_Element : constant Boolean := True;
   end Word_Byte_Array_Unconstrained_Text_IO;
end Adacore_8529_012_Aux;
