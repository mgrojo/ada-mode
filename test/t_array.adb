

procedure T_Array is
   type A is array (1 .. 10) of Integer;
   type B is
     array (1 .. 10) of Integer;
   type C is array (1..10) of Integer; --  wrongly indented

begin
   null;
end T_Array;

