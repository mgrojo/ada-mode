-------------------------------------------------------------------------------
--
--  Copyright (C) 2002, 2003, 2009 Stephe Leake
--  Copyright (C) 1999, 2000 FlightSafety International and Ted Dennison
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
--  This software was originally developed by the following company, and was
--  released as open-source software as a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body OpenToken.Token.Enumerated.Analyzer is

   type Match_List is array (Terminal_ID) of Recognizer.Analysis_Verdict;

   -------------------------------------------------------------------------
   --  type for handling token lookaheads.
   --
   type Token_List_Node is record
      Token_Handle : OpenToken.Token.Enumerated.Handle;
      Prev         : Token_List_Node_Pointer;
      Next         : Token_List_Node_Pointer;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Token_List_Node, Token_List_Node_Pointer);

   -------------------------------------------------------------------------
   --  Routines to handle indexes in a circular ring buffer
   --

   -----------------------------------------------------------------------
   --  Increment the given string buffer index by the given amount. If
   --  it ends up outside of the string bounds, it will be wrapped. To
   --  decrement it, suppy a negative number. The algorithm used
   --  relies on the fact that the string buffer will be ranged
   --  1..Max_String_Length.
   -----------------------------------------------------------------------
   function Increment_Buffer_Index
     (Index  : in Integer;
      Amount : in Integer := 1)
     return Natural
   is begin
      return ((Index + Amount - 1) mod Max_String_Length) + 1;
   end Increment_Buffer_Index;

   -----------------------------------------------------------------------
   --  Return True if the given index designates a valid element of
   --  the instance's ring buffer.
   -----------------------------------------------------------------------
   function In_Buffer
     (Index    : Integer;
      Analyzer : Instance)
     return Boolean
   is begin

      --  It is not in the buffer if the Index is outside the string or the string is empty
      if Index not in Analyzer.Buffer'Range or Analyzer.Buffer_Size = 0 then
         return False;
      end if;

      if Analyzer.Buffer_Head <= Analyzer.Buffer_Tail then
         --  It is in the buffer if its between the head and the tail
         return Index in Analyzer.Buffer_Head .. Analyzer.Buffer_Tail;
      else
         --  It is in the buffer if it is not bounded by the tail and the head
         return Index not in Analyzer.Buffer_Tail + 1 .. Analyzer.Buffer_Head - 1;
      end if;

   end In_Buffer;

   -----------------------------------------------------------------------
   --  Get more text for the analyzer's buffer. The actual amount
   --  retrieved depends on the amount available from the text feeder
   --  and either the amount of space left in the buffer, or the
   --  distance from the tail to the buffer's wrap point.
   -----------------------------------------------------------------------
   procedure Get_More_Text (Analyzer : in out Instance)
   is
      Old_Tail       : constant Natural := Analyzer.Buffer_Tail;
      First_New_Char : constant Natural := Increment_Buffer_Index (Analyzer.Buffer_Tail);
   begin
      if Analyzer.Buffer_Head > First_New_Char then

         OpenToken.Text_Feeder.Get
           (Feeder   => Analyzer.Feeder.all,
            New_Text => Analyzer.Buffer (First_New_Char .. Analyzer.Buffer_Head - 1),
            Text_End => Analyzer.Buffer_Tail);
      else
         OpenToken.Text_Feeder.Get
           (Feeder   => Analyzer.Feeder.all,
            New_Text => Analyzer.Buffer (First_New_Char .. Analyzer.Buffer'Last),
            Text_End => Analyzer.Buffer_Tail);
      end if;
      Analyzer.Buffer_Size := Analyzer.Buffer_Size +
        Increment_Buffer_Index (Analyzer.Buffer_Tail, -Old_Tail);

   end Get_More_Text;

   -----------------------------------------------------------------------
   --  Return the number of EOLs in the first Length characters in the
   --  Analyzer's buffer
   -----------------------------------------------------------------------
   function EOLs_Buffered
     (Analyzer : in Instance;
      Length   : in Natural)
     return Natural
   is
      Slice_Tail : constant Natural := Increment_Buffer_Index (Analyzer.Buffer_Head, Length - 1);
      EOL_String : constant String  := (1 => EOL_Character);
   begin
      if Slice_Tail < Analyzer.Buffer_Head then
         return Ada.Strings.Fixed.Count
           (Source  => Analyzer.Buffer (Analyzer.Buffer_Head .. Analyzer.Buffer'Last),
            Pattern => EOL_String)
           +
           Ada.Strings.Fixed.Count
           (Source  => Analyzer.Buffer (Analyzer.Buffer'First .. Slice_Tail),
            Pattern => EOL_String);
      else
         return
           Ada.Strings.Fixed.Count
           (Source  => Analyzer.Buffer (Analyzer.Buffer_Head .. Slice_Tail),
            Pattern => EOL_String);
      end if;
   end EOLs_Buffered;

   -----------------------------------------------------------------------
   --  Returns the number of characters found after the last EOL in
   --  the next Length characters after the buffer's head.
   -----------------------------------------------------------------------
   function Characters_After_Last_EOL
     (Analyzer : in Instance;
      Length   : in Natural)
     return Natural
   is
      Slice_Tail : constant Natural := Increment_Buffer_Index (Analyzer.Buffer_Head, Length - 1);
      EOL_String : constant String  := (1 => EOL_Character);

      Last_EOL : Natural;
   begin

      --  Find the last EOL character in the first Length characters
      --  of the buffer
      if Slice_Tail < Analyzer.Buffer_Head then
         Last_EOL := Ada.Strings.Fixed.Index
           (Source  => Analyzer.Buffer (Analyzer.Buffer'First .. Slice_Tail),
            Pattern => EOL_String,
            Going   => Ada.Strings.Backward);

         if Last_EOL = 0 then

            Last_EOL := Ada.Strings.Fixed.Index
              (Source  => Analyzer.Buffer (Analyzer.Buffer_Head .. Analyzer.Buffer'Last),
               Pattern => EOL_String,
               Going   => Ada.Strings.Backward);

         end if;
      else

         Last_EOL := Ada.Strings.Fixed.Index
           (Source  => Analyzer.Buffer (Analyzer.Buffer_Head .. Slice_Tail),
            Pattern => EOL_String,
            Going   => Ada.Strings.Backward);

      end if;

      --  Figure out how many characters are after that last EOL
      return Increment_Buffer_Index (Slice_Tail, 1 - Last_EOL);

   end Characters_After_Last_EOL;

   -------------------------------------------------------------------------
   --  Find all the characters that *aren't* part of a token match.
   --
   --  This routine should be called when the first character in the
   --  analyzer's buffer is unmatchable. It succesively checks each
   --  character thereafter until it finds one that *does* start a
   --  valid token.
   --
   --  Last_Unmatched will be set to the index of the last unmatchable
   --  character.
   -----------------------------------------------------------------------
   procedure Find_Non_Match
     (Unmatched_Length : out    Natural;
      Analyzer         : in out Instance)
   is

      --  The table of token matches
      Match : Match_List;

      Possible_Matches : Boolean;
      Current_Char     : Integer;

      use type OpenToken.Recognizer.Analysis_Verdict;
   begin

      --  Loop to find unrecognized characters
      Unmatched_Length := 1;

      Check_For_Unrecognized : loop

         --  Loop to see if the next chacter past the last unmatched one starts a valid token
         Current_Char := Increment_Buffer_Index (Analyzer.Buffer_Head, Unmatched_Length);

         --  Clear the state of all the tokens
         for Token_Index in Terminal_ID loop
            Recognizer.Clear (Analyzer.Syntax_List (Token_Index).Recognizer.all);
         end loop;
         Match := (others => Recognizer.So_Far_So_Good);

         Check_For_Match : loop

            --  Get more data when we run out
            if not In_Buffer (Index => Current_Char, Analyzer => Analyzer) then
               Get_More_Text (Analyzer);
            end if;

            --  Loop to see if this character starts a match on any
            --  token. We will assume that there are no possible
            --  matches until proven otherwise.
            Possible_Matches := False;

            for Token_Index in Terminal_ID loop

               if Match (Token_Index) /= Recognizer.Failed then

                  Recognizer.Analyze
                    (The_Token => Analyzer.Syntax_List (Token_Index).Recognizer.all,
                     Next_Char => Analyzer.Buffer (Current_Char),
                     Verdict   => Match (Token_Index));


               end if;

               case Match (Token_Index) is
                  --  If we found a match, quit.
                  when Recognizer.Matches =>
                     return;

                  --  If we *could* have a match, check the next character
                  when Recognizer.So_Far_So_Good =>
                     Possible_Matches := not (Current_Char = Analyzer.Buffer_Tail and then
                       OpenToken.Text_Feeder.End_Of_Text (Analyzer.Feeder.all));

                  when others =>
                     null;
               end case;

            end loop;

            exit Check_For_Match when not Possible_Matches;

            Current_Char := Increment_Buffer_Index (Current_Char);

         end loop Check_For_Match;

         Unmatched_Length := Unmatched_Length + 1;

         exit Check_For_Unrecognized when Current_Char = Analyzer.Buffer_Tail and then
           OpenToken.Text_Feeder.End_Of_Text (Analyzer.Feeder.all);

      end loop Check_For_Unrecognized;

   end Find_Non_Match;


   --------------------------------------------------------------------------
   --  Find the the best (aka: longest) match for the tokens in input
   --  stream The ID and length of the best token match will be
   --  returned.
   --
   --  This routine attempts to find the longest possible string in
   --  the Analyzer's buffer (starting at index 1) that matches a
   --  token. If the buffer runs out of characters during this
   --  process, it will be refilled from the Analyzer's text feeder
   --  function.
   --------------------------------------------------------------------------
   procedure Find_Best_Match
     (Analyzer          : in out Instance;
      Best_Match_Token  :    out Terminal_ID;
      Best_Match_Length :    out Natural)
   is

      --  The table of token matches
      Match : Match_List := (others => Recognizer.So_Far_So_Good);

      More_Possible_Matches : Boolean := True;

      Current_Char : Integer := Analyzer.Buffer_Head;

      use type OpenToken.Recognizer.Analysis_Verdict;
   begin

      --  Clear the state of all the tokens
      for Token_Index in Terminal_ID loop
         Recognizer.Clear (Analyzer.Syntax_List (Token_Index).Recognizer.all);
      end loop;

      Best_Match_Length     := 0;

      while More_Possible_Matches loop

         --  Get more text when we run out
         if not In_Buffer (Index => Current_Char, Analyzer => Analyzer) then
            Get_More_Text (Analyzer);
         end if;

         --  Assume no more matches until proven otherwise
         More_Possible_Matches := False;

         --  Check all the token Analyzers...
         for Token_Index in Terminal_ID loop

            --  check only tokens that haven't yet failed...
            if Match (Token_Index) /= Recognizer.Failed then

               --  Dispatch to the token's analyze routine with the new character
               Recognizer.Analyze
                 (The_Token => Analyzer.Syntax_List (Token_Index).Recognizer.all,
                  Next_Char => Analyzer.Buffer (Current_Char),
                  Verdict   => Match (Token_Index));

               --  If its the longest match yet, save it.
               if Match (Token_Index) = Recognizer.Matches and
                 Best_Match_Length < Increment_Buffer_Index (Current_Char, 1 - Analyzer.Buffer_Head) then

                  Best_Match_Token  := Token_Index;
                  Best_Match_Length := Increment_Buffer_Index (Current_Char, 1 - Analyzer.Buffer_Head);

               end if;

               --  If we find at least one possible match and we aren't at the end of the file,
               --  keep checking.
               if Match (Token_Index) /= Recognizer.Failed then
                  More_Possible_Matches := not (Current_Char = Analyzer.Buffer_Tail and then
                    OpenToken.Text_Feeder.End_Of_Text (Analyzer.Feeder.all));
               end if;
            end if;

         end loop;

         Current_Char := Increment_Buffer_Index (Current_Char);

      end loop;

   end Find_Best_Match;

   function Initialize
     (Language_Syntax : in Syntax;
      Feeder          : in Text_Feeder_Ptr := Input_Feeder'Access)
     return Instance
   is
      New_Analyzer : Instance;
   begin
      --  Initialize the syntax
      New_Analyzer.Syntax_List := Language_Syntax;
      for ID in Syntax'Range loop
         New_Analyzer.Syntax_List (ID).Token_Handle.ID := ID;
      end loop;

      New_Analyzer.Feeder        := Feeder;
      New_Analyzer.Has_Default   := False;

      New_Analyzer.Line        := 1;
      New_Analyzer.Column      := 1;
      New_Analyzer.Lexeme_Head := 1;
      New_Analyzer.Lexeme_Tail := 0;
      New_Analyzer.Buffer_Head := New_Analyzer.Buffer'First;
      New_Analyzer.Buffer_Tail := New_Analyzer.Buffer'Last;
      New_Analyzer.Buffer_Size := 0;
      New_Analyzer.Next_Line   := 1;
      New_Analyzer.Next_Column := 1;

      return New_Analyzer;
   end Initialize;

   function Initialize
     (Language_Syntax : in Syntax;
      Default         : in Terminal_ID;
      Feeder          : in Text_Feeder_Ptr := Input_Feeder'Access)
     return Instance
   is
      New_Analyzer : Instance;
   begin
      --  Initialize the syntax
      New_Analyzer.Syntax_List    := Language_Syntax;
      for ID in Syntax'Range loop
         New_Analyzer.Syntax_List (ID).Token_Handle.ID := ID;
      end loop;

      New_Analyzer.Feeder        := Feeder;
      New_Analyzer.Has_Default   := True;
      New_Analyzer.Default_Token := Default;

      New_Analyzer.Line        := 1;
      New_Analyzer.Column      := 1;
      New_Analyzer.Lexeme_Head := 1;
      New_Analyzer.Lexeme_Tail := 0;
      New_Analyzer.Buffer_Head := New_Analyzer.Buffer'First;
      New_Analyzer.Buffer_Tail := New_Analyzer.Buffer'Last;
      New_Analyzer.Buffer_Size := 0;
      New_Analyzer.Next_Line   := 1;
      New_Analyzer.Next_Column := 1;
      New_Analyzer.Lookahead_Queue := null;
      New_Analyzer.Lookahead_Head  := null;
      New_Analyzer.Lookahead_Tail  := null;

      return New_Analyzer;
   end Initialize;

   procedure Reset (Analyzer : in out Instance)
   is
      Prev : Token_List_Node_Pointer;
   begin
      Analyzer.Line        := 1;
      Analyzer.Column      := 1;
      Analyzer.Lexeme_Head := 1;
      Analyzer.Lexeme_Tail := 0;
      Analyzer.Buffer_Head := Analyzer.Buffer'First;
      Analyzer.Buffer_Tail := Analyzer.Buffer'Last;
      Analyzer.Buffer_Size := 0;
      Analyzer.Next_Line   := 1;
      Analyzer.Next_Column := 1;

      loop
         exit when Analyzer.Lookahead_Tail = null;
         Free (Analyzer.Lookahead_Tail.Token_Handle);
         Prev := Analyzer.Lookahead_Tail.Prev;
         Free (Analyzer.Lookahead_Tail);
         Analyzer.Lookahead_Tail := Prev;
      end loop;

      Analyzer.Lookahead_Head  := null;
      Analyzer.Lookahead_Queue := null;
   end Reset;

   procedure Set_Text_Feeder (Analyzer : in out Instance; Feeder : in Text_Feeder_Ptr) is
   begin
      Analyzer.Feeder      := Feeder;
      Analyzer.Line        := 1;
      Analyzer.Column      := 1;
      Analyzer.Next_Line   := 1;
      Analyzer.Next_Column := 1;
   end Set_Text_Feeder;

   procedure Set_Syntax (Analyzer : in out Instance; Language_Syntax : in Syntax)
   is begin
      --  This copies the pointers to recognizer, which is why this is
      --  unsafe.
      Analyzer.Syntax_List := Language_Syntax;

      --  If Language_Syntax was created with the Get in this package,
      --  using the default New_Token parameter, the Token IDs are all
      --  Token_ID'First, which is wrong. So fix that now.
      for ID in Syntax'Range loop
         Analyzer.Syntax_List (ID).Token_Handle.ID := ID;
      end loop;
   end Set_Syntax;

   function End_Of_Text (Analyzer : in Instance) return Boolean
   is begin
      return End_Of_Buffered_Text (Analyzer) and Text_Feeder.End_Of_Text (Analyzer.Feeder.all);
   end End_Of_Text;

   function End_Of_Buffered_Text (Analyzer : in Instance) return Boolean
   is begin
      return Analyzer.Buffer_Size = 0 or Analyzer.Buffer_Head = Analyzer.Buffer_Tail;
   end End_Of_Buffered_Text;

   procedure Discard_Buffered_Text (Analyzer : in out Instance)
   is begin
      Analyzer.Buffer_Head := Analyzer.Buffer'First;
      Analyzer.Buffer_Tail := Analyzer.Buffer'Last;
      Analyzer.Buffer_Size := 0;
   end Discard_Buffered_Text;

   procedure Set_Default
     (Analyzer : in out Instance;
      Default  : in     Terminal_ID)
   is begin
      Analyzer.Has_Default   := True;
      Analyzer.Default_Token := Default;
   end Set_Default;

   procedure Unset_Default (Analyzer : in out Instance)
   is begin
      Analyzer.Has_Default := False;
   end Unset_Default;

   overriding procedure Find_Next
     (Analyzer   : in out Instance;
      Look_Ahead : in     Boolean := False)
   is
      EOLs_Found : Integer;

      Matched_Token  : Terminal_ID;
      Matched_Length : Natural;

      Next : Token_List_Node_Pointer;

   begin

      --  Only read new tokens during lookaheads or when the lookahead list is empty
      if Look_Ahead or Analyzer.Lookahead_Queue = null then

         if Analyzer.Lookahead_Head = null then
            loop
               --  Find the best token match from the input stream
               Find_Best_Match
                 (Analyzer          => Analyzer,
                  Best_Match_Token  => Matched_Token,
                  Best_Match_Length => Matched_Length);

               --  If we didn't find a match, its a either syntax error
               --  or a match to the default token.

               if Matched_Length = 0 then
                  if Analyzer.Has_Default then

                     --  Find all the characters that *aren't* part of a
                     --  match
                     Find_Non_Match
                       (Unmatched_Length => Matched_Length,
                        Analyzer         => Analyzer);
                     Matched_Token := Analyzer.Default_Token;

                  else
                     raise Syntax_Error with "Unrecognized character '" & Analyzer.Buffer (Analyzer.Buffer_Head) & "'";
                  end if;
               end if;

               --  Update the line and column count

               Analyzer.Line   := Analyzer.Next_Line;
               Analyzer.Column := Analyzer.Next_Column;

               EOLs_Found := EOLs_Buffered (Analyzer => Analyzer, Length => Matched_Length);
               Analyzer.Next_Line := Analyzer.Next_Line + EOLs_Found;

               if EOLs_Found = 0 then
                  Analyzer.Next_Column := Analyzer.Next_Column + Matched_Length;
               else
                  Analyzer.Next_Column := 1 +
                    Characters_After_Last_EOL
                    (Analyzer => Analyzer,
                     Length   => Matched_Length);

               end if;

               --  Quit when we find a reportable token

               exit when Analyzer.Syntax_List (Matched_Token).Recognizer.Report;

               --  Ditch the last token to make room for more parsing

               Analyzer.Buffer_Head := Increment_Buffer_Index (Analyzer.Buffer_Head, Matched_Length);

               Analyzer.Buffer_Size := Analyzer.Buffer_Size - Matched_Length;

            end loop;

            --  Save off the information for the token we found
            Analyzer.Lexeme_Head := Analyzer.Buffer_Head;
            Analyzer.Lexeme_Tail := Increment_Buffer_Index (Analyzer.Buffer_Head, Matched_Length - 1);

            Create
              (Lexeme     => Lexeme (Analyzer),
               ID         => Matched_Token,
               Recognizer => Analyzer.Syntax_List (Matched_Token).Recognizer,
               New_Token  => Analyzer.Syntax_List (Matched_Token).Token_Handle.all);

            if Look_Ahead then
               if Analyzer.Lookahead_Tail = null then
                  --  Push the previous token on the lookahead queue
                  --  tail, so Push_Back can restore it if necessary.

                  Analyzer.Lookahead_Tail := new Token_List_Node'
                    (Token_Handle => new Enumerated.Class'(Analyzer.Syntax_List (Analyzer.Last_Token).Token_Handle.all),
                     Prev         => null,
                     Next         => null);
               end if;

               --  Push the matched token on the lookahead queue tail.

               Analyzer.Lookahead_Tail.Next := new Token_List_Node'
                 (Token_Handle => new Enumerated.Class'(Analyzer.Syntax_List (Matched_Token).Token_Handle.all),
                  Prev         => Analyzer.Lookahead_Tail,
                  Next         => null);

               Analyzer.Lookahead_Tail := Analyzer.Lookahead_Tail.Next;

               if Analyzer.Lookahead_Queue = null then
                  --  We want Lookahead_Queue to point to the first
                  --  token read with Look_Ahead True.
                  Analyzer.Lookahead_Queue := Analyzer.Lookahead_Tail;
               end if;

               Analyzer.Last_Token := Matched_Token;

               if Trace_Parse then
                  Trace_Put ("look ahead " & Token_ID'Image (Analyzer.Last_Token)); Ada.Text_IO.New_Line;
               end if;
            else
               Analyzer.Last_Token := Matched_Token;
            end if;

            --  Ditch the buffered lexeme to make room for more parsing
            Analyzer.Buffer_Head := Increment_Buffer_Index (Analyzer.Buffer_Head, Matched_Length);
            Analyzer.Buffer_Size := Analyzer.Buffer_Size - Matched_Length;
         else
            --  Read from lookahead_head
            Analyzer.Last_Token := Analyzer.Lookahead_Head.Token_Handle.ID;
            Analyzer.Syntax_List (Analyzer.Last_Token).Token_Handle.all := Analyzer.Lookahead_Head.Token_Handle.all;
            Analyzer.Lookahead_Head := Analyzer.Lookahead_Head.Next;

            if Trace_Parse then
               Trace_Put ("look ahead " & Token_ID'Image (Analyzer.Last_Token)); Ada.Text_IO.New_Line;
            end if;
         end if;
      else
         --  Read the next token from the lookahead queue head

         if Analyzer.Lookahead_Queue.Prev /= null then
            --  The first token pushed needs to be freed; it was there
            --  for Push_Back, which won't need it now.
            Free (Analyzer.Lookahead_Queue.Prev.Token_Handle);
            Free (Analyzer.Lookahead_Queue.Prev);
         end if;

         --  Keep track of Lookahead_Head in case the next call with
         --  Look_Ahead => True happens before the queue is emptied.
         if Analyzer.Lookahead_Queue = Analyzer.Lookahead_Head then
            Analyzer.Lookahead_Head := Analyzer.Lookahead_Queue.Next;
         end if;

         --  Pop the first item off the queue and put it into the syntax list
         Analyzer.Last_Token := Analyzer.Lookahead_Queue.Token_Handle.ID;
         Analyzer.Syntax_List (Analyzer.Last_Token).Token_Handle.all := Analyzer.Lookahead_Queue.Token_Handle.all;
         Next := Analyzer.Lookahead_Queue.Next;
         Free (Analyzer.Lookahead_Queue.Token_Handle);
         Free (Analyzer.Lookahead_Queue);
         Analyzer.Lookahead_Queue := Next;
         Analyzer.Lookahead_Queue.Prev := null;

         if Analyzer.Lookahead_Queue = null then
            Analyzer.Lookahead_Tail := null;
         end if;
      end if;
   end Find_Next;

   overriding procedure Push_Back (Analyzer : in out Instance; Count : in Integer)
   is begin
      if Trace_Parse then
         Trace_Put ("...push back" & Integer'Image (Count) & " ");
      end if;

      --  We must preserve the pushed back items in the queue; we are
      --  not reseting the text source, so they can't be recognized
      --  again.
      --
      --  If user calls with Count to high, they'll get Constraint_Error.

      for I in 1 .. Count loop
         if Analyzer.Lookahead_Head = null then
            --  Last_Token => last_token_read (with or without Look_Ahead => True)
            --  Lookahead_Tail => previous_token
            --  We want Last_Token to be previous_token, Lookahead_Head to be Last_Token
            Analyzer.Lookahead_Tail.Next := new Token_List_Node'
              (Token_Handle => new Enumerated.Class'(Analyzer.Syntax_List (Analyzer.Last_Token).Token_Handle.all),
               Prev         => Analyzer.Lookahead_Tail,
               Next         => null);

            Analyzer.Last_Token := Analyzer.Lookahead_Tail.Token_Handle.ID;

            Analyzer.Syntax_List (Analyzer.Last_Token).Token_Handle.all := Analyzer.Lookahead_Tail.Token_Handle.all;

            Analyzer.Lookahead_Tail := Analyzer.Lookahead_Tail.Next;
            Analyzer.Lookahead_Head := Analyzer.Lookahead_Tail;
         else
            Analyzer.Lookahead_Head := Analyzer.Lookahead_Head.Prev;

            Analyzer.Last_Token := Analyzer.Lookahead_Head.Token_Handle.ID;

            Analyzer.Syntax_List (Analyzer.Last_Token).Token_Handle.all := Analyzer.Lookahead_Head.Token_Handle.all;
         end if;

      end loop;

      if Trace_Parse then
         Ada.Text_IO.Put_Line ("; current token " & Token_ID'Image (Analyzer.Last_Token));
      end if;
   end Push_Back;

   function First_Column (Analyzer : in Instance) return Boolean is
   begin
      return Analyzer.Next_Column = 1;
   end First_Column;

   function Next_Token_Column (Analyzer : in Instance) return Integer is
   begin
      return Analyzer.Next_Column;
   end Next_Token_Column;

   function Line (Analyzer : in Instance) return Natural is
   begin
      return Analyzer.Line;
   end Line;

   function Column (Analyzer : in Instance) return Natural is
   begin
      return Analyzer.Column;
   end Column;

   overriding function Get (Analyzer : in Instance) return OpenToken.Token.Class
   is begin
      return Analyzer.Syntax_List (Analyzer.Last_Token).Token_Handle.all;
   end Get;

   ----------------------------------------------------------------------------
   --  Returns the last token ID that was matched.
   ----------------------------------------------------------------------------
   function ID (Analyzer : in Instance) return Terminal_ID is
   begin
      return Analyzer.Last_Token;
   end ID;

   overriding function Lexeme (Analyzer : in Instance) return String
   is begin
      if Analyzer.Lexeme_Tail = 0 then
         return "";
      end if;

      if Analyzer.Lexeme_Tail < Analyzer.Lexeme_Head then
         return Analyzer.Buffer (Analyzer.Lexeme_Head .. Analyzer.Buffer'Last) &
           Analyzer.Buffer (Analyzer.Buffer'First .. Analyzer.Lexeme_Tail);
      else
         return Analyzer.Buffer (Analyzer.Lexeme_Head .. Analyzer.Lexeme_Tail);
      end if;
   end Lexeme;

   function Get
     (Recognizer : in OpenToken.Recognizer.Class;
      New_Token  : in OpenToken.Token.Enumerated.Class := OpenToken.Token.Enumerated.Get)
     return Recognizable_Token
   is begin
      return (Recognizer   => new OpenToken.Recognizer.Class'(Recognizer),
              Token_Handle => new OpenToken.Token.Enumerated.Class'(New_Token)
              );
   end Get;

   overriding function Last_Recognizer (Analyzer : in Instance) return Recognizer_Handle
   is begin
      return Analyzer.Syntax_List (Analyzer.Last_Token).Recognizer;
   end Last_Recognizer;

end OpenToken.Token.Enumerated.Analyzer;
