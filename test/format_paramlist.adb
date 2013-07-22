--  Test ada-format-paramlist in pathological cases

package body Format_Paramlist is

   -- FIXME (later): delete the extra space in B default exp
   --EMACSCMD:(progn (forward-line 3)(forward-word 1)(insert "   ") (ada-align))
   -- result checked by diff
   procedure X (Y : in     Z 'Class := Default_Z;
                B : access Integer     ;
                A :    out Integer)
   is begin
      null;
   end;

   --FIXME (later): broken EMACSCMD:(progn (forward-line 3)(forward-word 1)(insert "   ") (ada-align))
   -- result checked by diff
   procedure Toto (D : in Integer;
                   C : in Integer) is
   begin
      null;
   end Toto;

   --FIXME (later): broken EMACSCMD:(progn (forward-line 3)(forward-word 1)(insert "   ") (ada-align))
   -- result checked by diff
   function F (D : in Z 'Class;
               C : in out  Z 'Class) return Integer is
   begin
      return 0;
   end F;
end Format_Paramlist;
