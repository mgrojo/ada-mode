--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (GPL);

package body WisiToken.Parser.LR.McKenzie_Recover is

   subtype McKenzie_Data is Recover_Data;
   Default_McKenzie : McKenzie_Data renames Default_Recover;

   function Recover
     (Parser        : in out LR.Instance'Class;
      Parsers       : in out Parser_Lists.List;
      Current_Token : in out Token_ID)
     return Boolean
   is
      pragma Unreferenced (Current_Token);

      Trace : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;

      Keep_Going : Boolean := False;
      Failed     : Boolean := False;
   begin
      Keep_Going := False;

      for I in Parsers.Iterate loop
         declare
            Cursor : constant Parser_Lists.Cursor := Parser_Lists.To_Cursor (Parsers, I);
         begin
            Cursor.Set_Recover (new McKenzie_Data'(Default_McKenzie));
         end;
      end loop;

      Matching_Input :
      loop
         for I in Parsers.Iterate loop
            declare
               use Parser_Lists;

               --  Cursor : constant Parser_Lists.Cursor := To_Cursor (Parsers, I);
               --  Data   : McKenzie_Data renames Mckenzie_Data (Cursor.Recover_Ref.Element.all);

            begin
               Failed := True;
            end;
         end loop;

         exit Matching_Input when Keep_Going or Failed;

      end loop Matching_Input;

      if Trace_Parse > 0 then
         if not Keep_Going then
            Trace.Put_Line ("recover: fail");
         end if;
      end if;
      return Keep_Going;
   end Recover;

end WisiToken.Parser.LR.McKenzie_Recover;
