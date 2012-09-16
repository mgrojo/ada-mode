
with Ada.Text_Io;
procedure Which_Test is

   procedure Titi is
      package Tata is new Ada.Text_Io.Integer_Io (Integer);
      --EMACSCMD: (which-function)
      --EMACSRESULT: "Titi"
   begin         --  01/03/2000: which-function says "Tata" from here.
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
      --EMACSCMD: (which-function)
      --EMACSRESULT: "T"
      null;  --  01/03/2000: which-function says "which_function" here.
   end T;  --  01/03/2000: when pressing enter, got an error in
           --  ada-goto-matching-decl-start

begin
   null;
end Which_Test;
