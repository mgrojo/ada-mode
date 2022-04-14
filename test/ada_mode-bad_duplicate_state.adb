-- This used to report a syntax error on 'renames', because the
-- duplicate state check in parse was not robust enough.
-- EMACASCMD:(switch-to-lr1)
procedure Ada_Mode.Bad_Duplicate_State
is
   use Children_Vectors;
   Ret_Vec : Vector;
   TDH     : Token_Data_Handler renames Node;
begin
   null;
end Ada_Mode.Bad_Duplicate_State;
