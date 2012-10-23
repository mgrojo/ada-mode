with Ada.Containers.Vectors;
procedure ada_mode.quantified_expressions is

begin

   -- Example code from Georg Bauhaus
   -- FIXME: add newlines inside parens, required declarations.
   if (for Some J in 1 .. 10 => J/2 = 0) then
      loop
         exit;
      end loop;
   end if;

   while (for Some J in 1 .. 10 => J/2 = 0) loop
      exit;
   end loop;

   case (for Some J in 1 .. 10 => J/2 = 0) is
      when others =>
      Label:
         declare
            generic
               Test : in Boolean;
            package Inner is
               procedure Run_Test;
            end Inner;

            package body Inner is
               procedure Run_Test
               is begin
                  Label:
                  loop
                     exit Label when Test;
                  end loop Label;
               end Run_Test;
            end Inner;
           
            package Instance is new Inner ((for some J in 1 .. 10 => True));
            -- FIXME: "some" not highlighted

         begin
            loop
               Instance.Run_Test;
               exit;
            end loop;
         end Label;
   end case;
   
   declare
      B : Boolean := False;
      function "+" (Item : in Integer) return access Integer
      is begin
         return new Integer'(Item);
      end;

      V : constant array (1 .. 10) of access Integer :=
        (+1, +2, +3, +4, +5, +6, +7, +8, +9, +10);
   begin
      while B
      loop
         -- FIXME: add newlines inside parens
         B := (for all J in 1 .. 10 => V (J).all/2 = 0);
      end loop;
   end;

   declare
      package Float_Vectors is new Ada.Containers.Vectors (Positive, Float);
      Board : Float_Vectors.Vector;
   begin
      for Element
         of Board loop
            Element := Element * 2.0;
         end loop;

      for Element of reverse Board
         loop
            Element := Element * 2.0;
         end loop;

      for Element : Float of
        Board
         loop
            Element := Element * 2.0;
         end loop;
      for Element
        : Float of reverse Board
         loop
            Element := Element * 2.0;
         end loop;
      for Element : Float
        of reverse Board
         loop
            Element := Element * 2.0;
         end loop;
   end;

end ada_mode.quantified_expressions;
