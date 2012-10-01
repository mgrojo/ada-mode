-- These commands are executed before the buffer is indented, so they
-- affect the whole file.
--
--EMACSCMD: (setq ada-indent-with 0)
package body Options is

   type Discrete_Type_1 is (A, B, C);
   Local_1 : Discrete_Type_1 := C;
begin

   case Local_1 is
   when A =>
      Local_1 := B;

   when B =>
      Local_1 := C;

   when C =>
      Local_1 := A;

   end case;

end Options;
