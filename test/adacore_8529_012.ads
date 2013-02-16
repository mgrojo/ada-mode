--EMACSCMD:(setq skip-recase-test t)
with Adacore_8529_012_Aux; use Adacore_8529_012_Aux;
package Adacore_8529_012 is

   procedure Put
     (Item                      : in Word_Byte_Array_Unconstrained;
      Single_Line_Array         : in Boolean := Word_Byte_Array_Unconstrained_Text_IO.Default_Single_Line_Array;
      Named_Association_Array   : in Boolean := Word_Byte_Array_Unconstrained_Text_IO.Default_Named_Association_Array;
      Single_Line_Element       : in Boolean := Word_Byte_Array_Unconstrained_Text_IO.Default_Single_Line_Element;
      Named_Association_Element : in Boolean :=
        Word_Byte_Array_Unconstrained_Text_IO.Default_Named_Association_Element);

end Adacore_8529_012;
