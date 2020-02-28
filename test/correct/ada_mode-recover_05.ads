package Ada_Mode.Recover_5 is

   type Result (Label : Result_Label) is record
      case Label is
         when Success =>
            Value     : Integer;
            Remaining : Ada.Strings.Unbounded.Unbounded_String;
         when Failure =>
            null;
      end case;
   end record;

   function Additive (Input : String) return Result;

end Ada_Mode.Recover_5;
