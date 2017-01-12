-- tests slices; example of typical code.

--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")

--EMACSCMD:(setq skip-recase-test t)
with Ada.Text_IO; use Ada.Text_IO;
procedure Ada_Mode.Slices is
   type Day is (Sun, Mon, Tues);

   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(forward-char 1)(ada-goto-declaration nil)(looking-at "+\" (Left : in Day; Right : in Integer) return Day$"))
   --EMACSRESULT:t
   function "+" (Left : in Day; Right : in Integer) return Day;

   function "+" (Left : in Day; Right : in Integer) return Day
   is begin
      return Sun;
   end "+";

   function "-" (Left, Right : in Day) return Integer
   is begin
      return 0;
   end "-";

   --  monadic + for testing ada-goto-declaration
   function "+" (Item : in Day) return Day
   is begin
      return Item;
   end "+";

   -- FIXME: ada-goto-declaration fails with ada-xref-tool = gnat and
   -- gnat version > GPL 2015; need a way to test with < GPL 2015.
   --
   --EMACSCMD:(progn (end-of-line 9)(backward-char 5)(ada-identifier-at-point))
   --EMACSRESULT: "\"+\""
   --EMACSCMD:(progn (end-of-line 7)(backward-char 2)(ada-identifier-at-point))
   --EMACSRESULT: "Sun"
   --EMACSCMD:(progn (end-of-line 5)(backward-char 3)(ada-identifier-at-point))
   --EMACSRESULT: "Sun"
   --EMACSCMD:(progn (end-of-line 3)(backward-char 4)(ada-identifier-at-point))
   --EMACSRESULT: "Sun"
   D1, D2 : Day := +Sun;
   --EMACSCMD:(progn (end-of-line 0)(backward-char 5)(ada-goto-declaration nil)(looking-at "+\" (Item"))
   --EMACSRESULT: t
   --EMACSCMD:(progn (end-of-line -2)(backward-char 4)(ada-goto-declaration nil)(looking-at "Sun, Mon,"))
   --EMACSRESULT: t

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
