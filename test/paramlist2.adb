
--  Test ada-format-paramlist
--  Spaces in the type name were incorrectly parsed

procedure X (Y : in Z 'Class    := 2432;
             B    :  access Integer     ;
             A : out Integer           := 2);

procedure Toto (D : in Integer;
                C : in Integer) is
begin
   null;
end Toto;

function F (D : in A 'Class;
            C : in out  A 'Class) return Integer is
begin
   null;
end F;


