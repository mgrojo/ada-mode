package body Ada_Mode.Parens is
   function Function_1
     (Param_1, Param_2 : in Ada.Text_IO. Count;
      Param_3 : in out Integer;
      Param_4 : in Float)
     return Float
   is
      Local_1 : Integer := (1 + 2 + 3);
      Local_2 : Integer := (1 + 2 +
                              3);
      Local_3 : Integer := (1 + 2
                              + 3);
      Local_4 : Integer := (1 +
                              2 + 3);
      Local_5 : Integer :=
        (1 + 2 +
           3);

      Local_6 : String := ("123" & "456" & "789");
      Local_7 : String := ("123" & "456" &
                             "789");
      Local_8 : String := ("123" &
                             "456" & "789");

      Local_9 : String := (
                           "123" &
                             "456" &
                             "789"
                          );

      Local_10 : String :=
        (
         "123" &
           "456" &
           "789"
        );

   begin
      return Float (
                    Integer'Value
                      (Local_6));
   end Function_1;

   function Function_2 (Left, Right : in Array_Type_1) return Array_Type_1
   is begin
      return
        (1 => 1,
         2 =>
           1 + 2 * 3,
         3 => 1 +
           3 * 4,
         others => 5);
   end;

   procedure If_Statement
     (A : in Boolean;
      B : in Boolean;
      C : in Boolean;
      D : in Boolean;
      E : in Boolean;
      G : in Boolean)
   is
   begin

      if A
        or else B
        or else C
      then
         null;
      end if;

      if A
        or else (B
                   and then C
                   and then D)  --  requires ada-indent-validate-cache-paren
      then
         null;
      end if;

      if A
        or else (B
                   and then C
                   and then D)
        or else ((B
                    and then C)
                   or else
                   (D
                      and then E))
        or else G
      then
         null;
      end if;

      while A
        or else B
      loop
         null;
      end loop;

      while A
        or else (B
                   and then C
                   and then D)
      loop
         null;
      end loop;

      while A
        or else (B
                   and then C
                   and then D)
        or else ((B
                    and then C)
                   or else
                   (D
                      and then E))
        or else G
      loop
         null;
      end loop;

      loop
         exit when A
           or else B
           or else C;

         exit when A
           or else (B
                      and then C
                      and then D); --  Indented on 'exit' instead of 'and then'

         exit when A
           or else (B
                      and then C
                      and then D)  --  Indented on 'if', instead of 'and then'
           or else ((B
                       and then C)
                      or else
                      (D
                         and then E))
           or else G;
      end loop;

   end If_Statement;

   procedure Hello
   is
      Hello : constant String := "hello";
      There  : constant String := " there";
      Out_File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Put_Line ("Hello" & ' ' & -- test ada-indent-next keyword with string, character literal
                              "World");

      Ada.Text_IO.Put_Line (Out_File, -- smie-backward-sexp returns "(" here
                            Hello &
                              There);
   end Hello;

end Ada_Mode.Parens;
