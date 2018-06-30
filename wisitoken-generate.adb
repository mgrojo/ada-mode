--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

pragma License (Modified_GPL);

with Ada.Directories;
with Ada.Strings.Fixed;
package body WisiToken.Generate is

   function Error_Message
     (File_Name : in String;
      File_Line : in Line_Number_Type;
      Message   : in String)
     return String
   is
      use Standard.Ada.Directories;
      use Standard.Ada.Strings.Fixed;
      use Standard.Ada.Strings;
   begin
      return Simple_Name (File_Name) & ":" &
        Trim (Line_Number_Type'Image (File_Line), Left) & ":0: " & Message;
   end Error_Message;

   procedure Put_Error (Message : in String)
   is begin
      Error := True;
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
   end Put_Error;

   procedure Check_Consistent
     (Grammar          : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor       : in WisiToken.Descriptor'Class;
      Source_File_Name : in String)
   is begin
      if Descriptor.Accept_ID /= Descriptor.First_Nonterminal then
         Put_Error
           (Error_Message
              (Source_File_Name, Line_Number_Type'First,
               "Descriptor.Accept_ID /= Descriptor.First_Nonterminal"));
      end if;
      if Grammar.First_Index /= Descriptor.First_Nonterminal then
         Put_Error
           (Error_Message
              (Source_File_Name, Line_Number_Type'First,
               "Grammar.First_Index /= Descriptor.First_Nonterminal"));
      end if;
      if Grammar.Last_Index /= Descriptor.Last_Nonterminal then
         Put_Error
           (Error_Message
              (Source_File_Name, Line_Number_Type'First,
               "Grammar.Last_Index /= Descriptor.Last_Nonterminal"));
      end if;

      for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         if Grammar (Nonterm).LHS /= Nonterm then
            Put_Error
              (Error_Message
                 (Source_File_Name, Line_Number_Type'First,
                  "Grammar (" & Image (Nonterm, Descriptor) & ").LHS /= " &
                    Image (Nonterm, Descriptor)));
         end if;
      end loop;
   end Check_Consistent;

   function Has_Empty_Production (Grammar : in WisiToken.Productions.Prod_Arrays.Vector) return Token_ID_Set
   is
      use all type Ada.Containers.Count_Type;

      subtype Nonterminal is Token_ID range Grammar.First_Index .. Grammar.Last_Index;

      Result  : Token_ID_Set := (Nonterminal => False);
      Changed : Boolean      := True;
   begin
      loop
         exit when not Changed;
         Changed := False;

         for Prod of Grammar loop
            for RHS of Prod.RHSs loop
               if (RHS.Tokens.Length = 0 or else
                     (RHS.Tokens (1) in Nonterminal and then Result (RHS.Tokens (1)))) and
                 not Result (Prod.LHS)
               then
                  Result (Prod.LHS) := True;
                  Changed := True;
               end if;
            end loop;
         end loop;
      end loop;
      return Result;
   end Has_Empty_Production;

end WisiToken.Generate;
