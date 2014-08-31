--  Test ada-format-paramlist in pathological cases
--  All results checked by diff

package body Format_Paramlist is

   -- FIXME (later): delete the extra space in B default exp
   --EMACSCMD:(progn (forward-line 3)(forward-word 1)(insert "   ") (ada-align))
   procedure X (Y : in     Z 'Class := Default_Z;
                B : access Integer     ;
                A :    out Integer)
   is begin
      null;
   end;

   --EMACSCMD:(progn (forward-line 3)(forward-word 1)(insert "   ") (ada-align))
   procedure Toto (D : in Integer;
                   C : in Integer) is
   begin
      null;
   end Toto;

   --EMACSCMD:(progn (forward-line 3)(forward-word 1)(insert "   ") (ada-align))
   function F (D : in     Z'Class;
               C : in out Z 'Class) return Integer is
   begin
      return 0;
   end F;


   --EMACSCMD:(progn (forward-line 3)(forward-word 1)(insert "   ") (ada-align))
   --  just 'out'
   function G (D : out Z'Class;
               C : out Z 'Class) return Integer
   is begin
      D.Z_Int := 1;
      C.Z_Int := 2;
      return 0;
   end G;

   --  Handle 'aliased' (Ada 2012 syntax)
   procedure H
     (A : aliased in              Z;
      B :            out          Z;
      C : aliased access          Z;
      D : aliased not null access Z)
   is begin
      null;
   end H;

end Format_Paramlist;
