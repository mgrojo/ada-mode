package body Ada_Mode.Recover_Navigate_1 is
   procedure Add_Param (Name : in String; Value : in String)
   is begin
      if Need_Comma then
         Statement := Statement & ", ";
      end if;
      Need_Comma := True;

      if A then
      Statement := Statement & Name & " = ?";
      else
         Statement := Statement & Name;
         Values    := Values & "?";
      end if;
      Last := Last + 1;
      Params (Last) := +Value;
   end Add_Param;
end Ada_Mode.Recover_Navigate_1;
