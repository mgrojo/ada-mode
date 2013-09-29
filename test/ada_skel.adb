--  test ada-skel.el; does not compile (not even close :)
--
--  We expand each token in ada-skel-token-alist, after first deleting
--  the expected expansion. The result is checked by diff.
package body Ada_Skel is

   --  skeletons in ada-skel function alphabetical order, except split
   --  into declarations and statements (the ada-wisi parser insists on
   --  that)

   --  ada-skel-record
   --EMACSCMD:(progn (forward-line 1)(kill-word 5)(forward-line 1)(kill-line 1)(forward-char -1)(insert " Record_Type_1")(funcall ada-expand))
   type Record_Type_1 is record
   end record;

   --  ada-skel-package
   --EMACSCMD:(progn (forward-line 1)(forward-word 3)(kill-word 1)(forward-line 1)(kill-line 2)(forward-char -1)(funcall ada-expand))
   package Package_1 is
   private
   end Package_1;

   --  ada-skel-protected
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-word 1)(forward-word 2)(kill-word 1)(forward-line 1)(kill-line 2)(forward-char -1)(funcall ada-expand))
   protected type Protected_1 is
   private
   end Protected_1;

   --  ada-skel-task
   --EMACSCMD:(progn (forward-line 1)(forward-word 3)(kill-word 1)(forward-line 1)(kill-line 1)(forward-char -1)(funcall ada-expand))
   task Task_1 is
   end Task_1;

   --  ada-skel-return
   function Function_1 return Integer
   is begin
      --EMACSCMD:(progn (forward-line 1)(forward-word 1)(forward-char 1)(kill-line 2)(forward-char -1)(funcall ada-expand))
      return
      do
      end return;

   end Function_1;

   --  ada-skel-entry
   protected body Protected_1 is

      --EMACSCMD:(progn (forward-line 1)(forward-word 3)(kill-word 1)(forward-char 1)(kill-line 3)(forward-char -1)(funcall ada-expand))
      entry Entry_1 when
      is
      begin
      end Entry_1;

   end Protected_1;

begin

   --  ada-skel-accept
   --EMACSCMD:(progn (forward-line 1)(forward-word 3)(kill-word 1)(forward-char 1)(kill-line 1)(forward-char -1)(funcall ada-expand))
   accept Accept_1 do
   end Accept_1;

   --  ada-skel-case
   --EMACSCMD:(progn (forward-line 1)(forward-word 2)(kill-word 1)(forward-char 1)(kill-line 2)(forward-char -1)(funcall ada-expand))
   case A is
      when =>
   end case;

   --  ada-skel-declare with block name
   --EMACSCMD:(progn (forward-line 1)(kill-line 1)(forward-word 1)(forward-line 1)(kill-line 3)(forward-char -1)(insert " Block_1")(funcall ada-expand))
Block_1:
   declare
   begin
   exception
   end Block_1;

   --  ada-skel-declare without block name
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(forward-line 1)(kill-line 3)(forward-char -1)(funcall ada-expand))
   declare
   begin
   exception
   end;

   --  ada-skel-for with name
   --EMACSCMD:(progn (forward-line 1)(kill-line 1)(forward-word 1)(kill-word 1)(forward-line 1)(kill-line 1)(forward-char -1)(insert " Loop_1")(funcall ada-expand))
Loop_1:
   for  loop
   end loop Loop_1;

   --  ada-skel-for without name
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-word 1)(forward-line 1)(kill-line 1)(forward-char -1)(funcall ada-expand))
   for  loop
   end loop;

   --  ada-skel-if
   --EMACSCMD:(progn (forward-line 1)(forward-word 2)(forward-line 1)(kill-line 3)(forward-char -1)(funcall ada-expand))
   if  then
   elsif  then
   else
   end if;

   --  ada-skel-loop with name
   --EMACSCMD:(progn (forward-line 1)(kill-line 1)(forward-word 1)(forward-line 1)(kill-line 2)(forward-char -1)(insert " Loop_1")(funcall ada-expand))
Loop_1:
   loop
      exit Loop_1 when ;
   end loop Loop_1;

   --  ada-skel-select
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(forward-line 1)(kill-line 2)(forward-char -1)(funcall ada-expand))
   select
   else
   end select;

   --  ada-skel-while with name
   --EMACSCMD:(progn (forward-line 1)(kill-line 1)(forward-word 1)(kill-word 1)(forward-line 1)(kill-line 1)(forward-char -1)(insert " Loop_1")(funcall ada-expand))
Loop_1:
   while  loop
   end loop Loop_1;

   --  ada-skel-while without name
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-word 1)(forward-line 1)(kill-line 1)(forward-char -1)(funcall ada-expand))
   while  loop
   end loop;

end Ada_Skel;
