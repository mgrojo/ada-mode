-- tests slices; example of typical code.

--EMACSCMD:(setq skip-recase-test t)
with Ada.Text_IO; use Ada.Text_IO;
procedure Ada_Mode.Slices is
   type Day is (Sun, Mon, Tues);

   function "+" (Left : in Day; Right : in Integer) return Day;
   function "-" (Left, Right : in Day) return Integer;

   D1, D2 : Day;
   N      : Integer;
   Line   : String(1..80);
   Last   : Natural;
begin
   Put_Line("When prompted for a Day value, enter one of: ");
   for D in Day loop
      Put(Day'Image(D) & "  ");
   end loop;
   New_Line;
   Put_Line("Starting tests for Day+Integer...");
   loop
      Put("Type a Day (empty line to exit): ");
      Get_Line(Line, Last);
      exit when Last=0;
      begin
         D1 := Day'Value(Line(1..Last));
      exception
         when Constraint_Error =>
            D1 := Sun;
            Put_Line("Input is invalid, set to sunday");
      end;
      loop
         begin
            Put("Type an integer: ");
            Get_Line(Line, Last);
            N := Integer'Value(Line(1..Last));
            Put_Line("Printing N" &Integer'Image(N));

            if N < 0  then
               Put_Line("Value set to 0, since entered value was wrong");
               N := 0;
            end if;
            exit;
         exception
            when Constraint_Error =>
               Put_Line ("Enter a integer between greater than 0");
         end;
      end loop;

      D2 := D1 + N;  -- The code being tested
      Put_Line("Result is " & Day'Image(D2));
   end loop;
   Put_Line("Starting tests for Day-Day...");
   loop
      Put("Type a Day (empty line to exit): ");
      Get_Line(Line, Last);
      exit when Last=0;
      begin
         D1 := Day'Value(Line(1..Last));
      exception
         when Constraint_Error =>
            D1 := Sun;
            Put_Line("Input is invalid, set to sunday");
      end;

      Put("Type a Day (empty line to exit): ");
      Get_Line(Line, Last);
      exit when Last=0;
      begin
         D2 := Day'Value(Line(1..Last));
      exception
         when Constraint_Error =>
            D2 := Sun;
            Put_Line("Input is invalid, set to sunday");
      end;

      N := D1 - D2;  -- The code being tested
      Put_Line(Day'Image(D1) & " - " & Day'Image(D2) & " = " &
                 Integer'Image(N));
   end loop;
end Ada_Mode.Slices;
