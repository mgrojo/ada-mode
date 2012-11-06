procedure Ada_Mode.Conditional_Expressions is
   subtype Bounded is Integer range -1 .. +1;
   J : Integer := 42;
   K : Integer := (if J > 42
                   then -1
                   else +1);
   L : Integer := (case J is
                      when 42 => -1,
                      when Integer'First .. 41 => 0,
                      when others => +1);
   M : Integer;
begin
   K := (if K < 0
         then 42
         elsif K = 0
         then (case J is
                  when 42 => -1,
                  when Integer'First .. 41 => 0,
                  when others => +1)
         else 44);
   K := (if K < 0
         then 42
         elsif K = 0
         then 42
         else (if J > 42
               then 44
               else 45));
   K := (case Bounded (K) is
            when -1 => 42,
            when 0 => 41,
            when 1 => 43);
   L := (case Bounded (K) is
            when -1 => 42,
            when 0 => 41,
            when 1 => (if J > 42
                       then 44
                       else 45));
end Ada_Mode.Conditional_Expressions;
