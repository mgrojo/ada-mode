-- From a real editing session. Inserting text with '\' encountered an
-- error in Edit_Source.

pragma License (Modified_Gpl);

with Ada.Directories;
with Ada.Finalization;
with Ada.Text_Io;
with Gnat.Os_Lib;
with Sal.Gen_Unbounded_Definite_Red_Black_Trees;
package body Wisi.Parse_Context is


   --EMACSCMD:(progn (forward-line 1)(set-mark-command nil)(forward-line 80)(kill-region nil nil t))
   function Image (Item : in Change) return String
   is
      use Wisitoken;
   begin
      return "(" &
        Item.Begin_Byte_Pos'Image & "," &
        Item.Begin_Char_Pos'Image & "," &
        Item.Inserted_End_Byte_Pos'Image & "," &
        Item.Inserted_End_Char_Pos'Image & "," &
        " +""" & (-Item.Inserted_Text) & """," &
        Item.Deleted_Bytes'Image & "," &
        Item.Deleted_Chars'Image & ")";
   end Image;

   function Get_Emacs_Change_List
     (Command_Line : in     String;
      Last         : in out Integer)
     return Change_Lists.List
   is
      function Substitute_Escapes (Item : in String) return String
      is begin
         if Item'Length = 0 then
            return Item;
         else
            declare
               I      : Integer := Item'First;
               J      : Integer := Item'First;
               Result : String (Item'Range);
            begin
               loop
                  if Item (I) = '\' and I < Item'Last then
                     if Item (I + 1) = 'n' then
                        Result (J) := Ascii.Lf;
                        I := @ + 2;
                     elsif Item (I + 1) = '"' then
                        Result (J) := '"';
                        I := @ + 2;
                     else
                        Result (J) := Item (I);
                        I := @ + 1;
                     end if;
                  else
                     Result (J) := Item (I);
                     I := @ + 1;
                  end if;
                  exit when I > Item'Last;
                  J := @ + 1;
               end loop;
               return Result (Result'First .. J);
            end;
         end if;
      end Substitute_Escapes;

   begin
      return Result : Change_Lists.List do
         Skip (Command_Line, Last, '('); --  start of changes list
         loop
            exit when Last = Command_Line'Last;
            exit when Command_Line (Last + 1) = ')';

            declare
               use Wisitoken;
               Item : Change;
            begin
               Skip (Command_Line, Last, '(');
               Item.Begin_Byte_Pos        := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Begin_Char_Pos        := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Inserted_End_Byte_Pos := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Inserted_End_Char_Pos := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Deleted_Bytes         := Get_Integer (Command_Line, Last);
               Item.Deleted_Chars         := Get_Integer (Command_Line, Last);
               Item.Inserted_Text         := +Substitute_Escapes (Get_String (Command_Line, Last));
               Skip (Command_Line, Last, ')');

               Result.Append (Item);
            end;
         end loop;
         Skip (Command_Line, Last, ')'); --  end of edits list
      end return;
   end Get_Emacs_Change_List;

   --EMACSCMD:(progn (forward-line 12)(insert " ")(indent-for-tab-command))
   --EMACSCMD:(progn (forward-line -2)(yank))
   --EMACSCMD:(progn (forward-line 10)(insert " ")(indent-for-tab-command))
   procedure Edit_Source
     (Trace            : in out Wisitoken.Trace'Class;
      Source           : in out Ada.Strings.Unbounded.String_Access;
      Source_Byte_Last : in out Integer;
      Source_Char_Last : in out Integer;
      Changes          : in     Change_Lists.List;
      Kmn_List         :    out Wisitoken.Parse.Kmn_Lists.List)
   is
      use Ada.Containers;
      use Wisitoken;

      --  Changes is in increasing time order (ie _not_ in buffer pos
      --  order); KMN_List is in buffer pos order.


   begin
      null;
   end Edit_Source;

end Wisi.Parse_Context;
