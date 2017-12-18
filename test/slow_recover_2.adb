--  Condensed from a real example of slow error recovery
   function Remove return Element_Type
   is
      X : Node_Access;
   begin
      X := Z.Child;
      loop

         X := X.Right;

--  Error here; meant to type 'end loop;'
         loop;

   end Remove;
