--  This file demos a problem with unusual layout & "not overriding".

procedure Ada_Mode.Not_Overriding is

   package P is

      type T is tagged null record;

      not
      overriding
      procedure P (Param : T);

      not
      overriding
      function F (Param : T)
                 return Integer;

   end P;

   package body P is

      not
      overriding
      procedure P (Param : T)
      is
      begin
         null;
      end P;

      not
      overriding
      function F (Param : T)
                 return Integer
      is
      begin
         return 42;
      end F;

   end P;

begin
   null;
end Ada_Mode.Not_Overriding;
