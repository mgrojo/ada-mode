procedure A is
   procedure Put_Line (A : in Integer; B : in String)
   is begin
      null;
   end;

   Adas      : Integer := 0;
   Fsfsdfsdf : constant String := "hello";
   Fsfdsfsd  : constant String := "there";
begin
   Put_Line (Adas,
             Fsfsdfsdf &
               Fsfdsfsd);

   Put_Line (Adas,
             B =>  Fsfsdfsdf &
               Fsfdsfsd);

   Put_Line (A => Adas,
             B => Fsfsdfsdf &
               Fsfdsfsd);
end A;
