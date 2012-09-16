package Adacore_8910_003_Aux is
   type Day is (Sun, Mon, Tues);

   function "+" (Left : in Day; Right : in Integer) return Day;
   function "-" (Left, Right : in Day) return Integer;
end Adacore_8910_003_Aux;
