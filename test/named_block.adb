
--  This file is intended to test named blocks indentation
--  With the adamode 3.2 and 3.3b, pressing <tab> multiple times
--  on the line following a label was just inserting a <tab>


procedure Named_Block is
   A : Integer;
begin
L1:
    for I in 1..5 loop
       null;
    end loop L1;
L2:
    for I in 1..5 loop
       null;
    end loop L2;

L3: for I in 1..5 loop
       null;
    end loop L3;

L4: for I in 1..5 loop
       null;
    end loop L4;

    A := 2;

BLOCK_TRY:
    declare
       INTERNAL_VAR : INTEGER := 100;
    begin
   TEST_LOOP:
       loop
          exit when A > 0;
          A := A - 1;
       end loop TEST_LOOP;
    end BLOCK_TRY;

BLOCK_TRY2:
    begin
   TEST_LOOP2:
       loop
          null;
       end loop TEST_LOOP2;
    end BLOCK_TRY2;

end Named_Block;
