--EMACSCMD:(font-lock-fontify-buffer)
with Ada.Containers.Vectors;
procedure Ada_Mode.Quantified_Expressions is

begin

   -- Example code from Georg Bauhaus

   if (for some J in 1 .. 10 => J/2 = 0) then
      null;
   end if;

   if (for some J in 1 .. 10 =>
         J/2 = 0)
   then
      null;
   end if;

   if (for some J in 1 ..
         10 => J/2 = 0)
   then
      null;
   end if;

   if (for some J in
         1 .. 10 => J/2 = 0)
   then
      null;
   end if;

   if (for some J
         in 1 .. 10 => J/2 = 0)
   then
      null;
   end if;

   if (for some
         J in 1 .. 10 => J/2 = 0)
   then
      null;
   end if;

   if (for
         some J in 1 .. 10 => J/2 = 0)
   then
      null;
   end if;

   declare
      package Float_Vectors is new Ada.Containers.Vectors (Positive, Float);
      Board : Float_Vectors.Vector;
   begin
      --EMACSCMD:(test-face "reverse" 'font-lock-keyword-face)
      for Element : Float of reverse Board
      loop
         Element := Element * 2.0;
      end loop;

      for Element : Float of reverse
        Board loop
         Element := Element * 2.0;
      end loop;

      for Element : Float of
        reverse Board loop
         Element := Element * 2.0;
      end loop;

      for Element : Float
        of reverse Board
      loop
         Element := Element * 2.0;
      end loop;

      for Element :
        Float of reverse Board
      loop
         Element := Element * 2.0;
      end loop;

      for Element
        : Float of reverse Board
      loop
         Element := Element * 2.0;
      end loop;

      for
        Element : Float of reverse Board
      loop
         Element := Element * 2.0;
      end loop;

      -- no ":", "reverse
      for Element of Board loop
         Element := Element * 2.0;
      end loop;

      for Element of Board
      loop
         Element := Element * 2.0;
      end loop;

      for Element of
        Board loop
         Element := Element * 2.0;
      end loop;

      for Element
        of Board loop
         Element := Element * 2.0;
      end loop;

      for
        Element of Board loop
         Element := Element * 2.0;
      end loop;

   end;

end Ada_Mode.Quantified_Expressions;
