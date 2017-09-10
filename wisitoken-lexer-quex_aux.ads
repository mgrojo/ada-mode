--  Abstract :
--
--  Non-generic package containing types for WisiToken.Lexer.Quex
--  generic parameters.
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

with Interfaces.C;
with System;
package WisiToken.Lexer.Quex_Aux is

   type Lexer_Type is new System.Address;

   type Token_Type is record
      --  Must match token type defined in .qx file output by wisi-generate;
      --  see Quex output *-token.h.
      --
      --  We don't return the character position in the Quex buffer, because
      --  there is no simple mapping back to the user encoding character
      --  position. Line, Column, Length do map.
      ID     : Interfaces.Unsigned_16;
      Length : Interfaces.C.size_t; -- in characters
      Line   : Interfaces.Unsigned_32;
      Column : Interfaces.Unsigned_16;
   end record
      with Convention => C;

   type Token_Access is access all Token_Type;

   --  Copied from quex-0.67.5/quex/code_base/definitions QUEX_TYPE_DEFINITIONS_E_ERROR
   type Error_Codes is
     (E_Error_Allocation_BufferMemory_Failed,
      E_Error_Allocation_ByteLoader_Failed,
      E_Error_Allocation_LexatomLoader_Failed,
      E_Error_Buffer_CannotAbsorbMoreContent,
      E_Error_Constructor_Accumulator_Failed,
      E_Error_Constructor_Counter_Failed,
      E_Error_Constructor_PostCategorizer_Failed,
      E_Error_IncludePopOnEmptyStack,
      E_Error_IncludePush_OnError,
      E_Error_InputName_Set_Failed,
      E_Error_NoHandler_OnBadLexatom,
      E_Error_NoHandler_OnEndOfStream,
      E_Error_NoHandler_OnFailure,
      E_Error_NoHandler_OnIndentationBad,
      E_Error_NoHandler_OnLoadFailure,
      E_Error_NoHandler_OnOverflow,
      E_Error_NoHandler_OnSkipRangeOpen,
      E_Error_None,
      E_Error_ProvidedExternal_Memory_Corrupt,
      E_Error_Reset_OnError,
      E_Error_TokenRepetitionNSetButNotSupportedByGeneratedCode,
      E_Error_TokenQueueNoMoreTokensAvailable,
      E_Error_Uninitialized,
      E_Error_UnitTest_Termination,
      E_Error_UserConstructor_Failed,
      E_Error_UserMementoPack_Failed,
      E_Error_Userreset_Failed)
   with Convention => C;

end WisiToken.Lexer.Quex_Aux;
