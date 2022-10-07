with Ada.Text_Io;
procedure Which_Test is

   procedure Titi is
      package Tata is new Ada.Text_Io.Integer_Io (Integer);
      --EMACSCMD: (ada-which-function)
      --EMACSRESULT:(if wisi-parser-shared "Titi" "")
   begin
      null;
   end Titi;

   procedure Tutu is
   begin
      null;
   end Tutu;

   procedure T is
      procedure T2 is
         procedure T3 is
         begin
            null;
         end T3;
      begin
         null;
      end T2;
   begin
      --EMACSCMD: (ada-which-function)
      --EMACSRESULT: (if wisi-parser-shared "T" "")
      null;
   end T;

begin
   null;
end Which_Test;
