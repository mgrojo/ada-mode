
procedure Operator_Xref is

   type My_Integer is new Integer;

   function "<" (A, B : My_Integer) return Boolean is
   begin
      return True;
   end "<";

   A : My_Integer := 1;
   B : My_Integer := 2;
   C : Boolean;
begin
   C := A < B;
end Operator_Xref;
