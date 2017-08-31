--  Abstract :
--
--  Store token regions.
--
--  The parser deals only with token_ids; this package adds additional
--  information.
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

pragma License (Modified_GPL);

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Text_IO;
with SAL.Gen_Queue_Interfaces;
with SAL.Gen_Unbounded_Definite_Queues;
with WisiToken.Lexer;
with WisiToken.Token;
package WisiToken.Token_Region is

   type Token is new WisiToken.Augmented_Token with record
      Line   : Ada.Text_IO.Count;
      Col    : Ada.Text_IO.Count; -- valid if Line > 0
      Region : Buffer_Region;     -- valid if Line = 0
   end record;

   function Image
     (Descriptor : in WisiToken.Descriptor'Class;
      Item       : in Token;
      ID_Only    : in Boolean)
     return String;
   --  Return a string for debug/test messages

   Default_Token : constant Token := (Invalid_Token_ID, 0, 0, Null_Buffer_Region);

   package Token_Queue_Interfaces is new SAL.Gen_Queue_Interfaces (Token);
   package Token_Queues is new SAL.Gen_Unbounded_Definite_Queues (Token, Token_Queue_Interfaces);

   type Error_Data
     (First_Terminal : Token_ID;
      Last_Terminal  : Token_ID)
   is record
      Error_Token    : Token;
      Expecting      : Token_ID_Set (First_Terminal .. Last_Terminal);
      Invalid_Region : Buffer_Region;
      Recover        : access WisiToken.Token.Recover_Data'Class;
   end record;

   package Error_Data_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Error_Data);

   package Error_List_Arrays is new Ada.Containers.Vectors (Natural, Error_Data_Lists.List, Error_Data_Lists."=");
   --  IMPROVEME: parser_id is not < 15; parser ids are never reused. So
   --  this ends up with many empty slots, because first_index = 0. use a
   --  map, or a vector with arbitrary first_index.

   procedure Put
     (File_Name  : in String;
      Errors     : in Error_List_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class);
   --  Put user-friendly error messages to Ada.Text_IO.Current_Output.

   type State_Type is new WisiToken.Token.Semantic_State with record
      Stack : Augmented_Token_Array;
      --  Top of stack is Stack.Last_Index; Push = Append, Pop = Delete_Last.
      --  Tokens are added by Push_Token, removed by Merge_Tokens.
      --  FIXME: change to SAL.stack?

      Lookahead_Queue : Token_Queues.Queue_Type;

      Errors : Error_List_Arrays.Vector;
      --  Indexed by Parser_ID.
   end record;

   function Active_Error_List (State : not null access State_Type) return Error_List_Arrays.Constant_Reference_Type;
   --  Return a reference to the single active error list.
   --  If more than one is active, raise Programmer_Error.

   overriding
   procedure Put (State : not null access State_Type);

   overriding
   procedure Reset (State : not null access State_Type);

   overriding
   procedure Lexer_To_Lookahead
     (State : not null access State_Type;
      ID    : in              Token_ID;
      Lexer : not null access WisiToken.Lexer.Instance'Class);

   overriding
   procedure Error
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      Expecting : in              Token_ID_Set);

   overriding
   procedure Spawn
     (State         : not null access State_Type;
      Old_Parser_ID : in              Natural;
      New_Parser_ID : in              Natural);

   overriding
   procedure Terminate_Parser
     (State     : not null access State_Type;
      Parser_ID : in              Natural);

   overriding
   procedure Virtual_To_Lookahead
     (State : not null access State_Type;
      ID    : in              Token_ID);

   overriding
   procedure Push_Current
     (State : not null access State_Type;
      ID    : in              Token_ID);

   overriding
   procedure Reduce_Stack
     (State   : not null access State_Type;
      Nonterm : in              Token_ID;
      Index   : in              Natural;
      IDs     : in              WisiToken.Token.List.Instance;
      Action  : in              Semantic_Action);

   overriding
   procedure Discard_Lookahead
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      ID        : in              Token_ID);

   overriding
   procedure Discard_Stack
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      ID        : in              Token_ID);

   overriding
   procedure Recover
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      Recover   : in              WisiToken.Token.Recover_Data'Class);

end WisiToken.Token_Region;
