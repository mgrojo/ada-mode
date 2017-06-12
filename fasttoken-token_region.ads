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
with FastToken.Lexer;
with FastToken.Token;
with SAL.Gen_Definite_Queues;
package FastToken.Token_Region is

   type Token is new FastToken.Augmented_Token with record
      Region : Buffer_Region;
   end record;

   function Image
     (Descriptor : in FastToken.Descriptor'Class;
      Item       : in Token;
      ID_Only    : in Boolean)
     return String;
   --  Return a string for debug/test messages

   Default_Token : constant Token := (Token_ID'Last, Null_Buffer_Region);

   package Token_Queues is new SAL.Gen_Definite_Queues (Token);

   type Recover_Data
     (First_Terminal : Token_ID;
      Last_Terminal  : Token_ID)
   is record
      Error_Token    : Token;
      Expecting      : Token_ID_Set (First_Terminal .. Last_Terminal);
      Invalid_Region : Buffer_Region;
   end record;

   package Recover_Data_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Recover_Data);

   type State_Type is new FastToken.Token.Semantic_State with record
      Stack : Token_Stack_Type;
      --  Top of stack is Stack.Last_Index; Push = Append, Pop = Delete_Last.
      --  Tokens are added by Push_Token, removed by Merge_Tokens.

      Input_Queue : Token_Queues.Queue_Type (500);
      --  Tokens are kept in Input_Queue during parallel parser
      --  execution, and during error recovery; added by Input_Token,
      --  moved to Stack by Push_Token.
      --
      --  IMPROVEME: get default queue size from somewhere.
      --  Max_Stack_Size is a reasonable guess for the maximum queue
      --  needed; better would be to set it at run-time based on
      --  history of compiling a particular project.

      Invalid_Region : Buffer_Region;
      --  Temporary storage during recovery; Discard_Token increases
      --  this, Recover resets it.

      Recover : Recover_Data_Lists.List;
   end record;

   overriding
   procedure Reset (State : access State_Type);

   overriding
   procedure Input_Token
     (Token : in     Token_ID;
      State : access State_Type;
      Lexer : in     FastToken.Lexer.Handle);

   overriding
   procedure Push_Token
     (ID    : in     Token_ID;
      State : access State_Type);

   overriding procedure Error
     (Expecting : in     Token_ID_Set;
      State     : access State_Type);

   overriding
   procedure Discard_Token
     (ID    : in     Token_ID;
      State : access State_Type);

   overriding
   procedure Merge_Tokens
     (Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     FastToken.Token.List.Instance;
      Action  : in     Semantic_Action;
      State   : access State_Type);

   overriding
   procedure Recover
     (Popped_Tokens : in     FastToken.Token.List.Instance;
      Pushed_Tokens : in     FastToken.Token.List.Instance;
      State         : access State_Type);

end FastToken.Token_Region;
