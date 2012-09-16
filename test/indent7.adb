package body Indent7 is

   package Internal is

      function Is_Existing return Boolean;

      function Is_Existing (With_Args : Boolean) return Boolean;

   end Internal;


   package body Internal is separate;

end Indent7;  --  uncorrectly indented when we have "separate" above
