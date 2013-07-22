--  Let the body compile
package Format_Paramlist is

   type Z is tagged null record;

   Default_Z : constant Z := (others => <>);

   procedure X (Y : in     Z 'Class := Default_Z;
                B : access Integer;
                A :    out Integer);

end Format_Paramlist;
