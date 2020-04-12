--  test ada-skel.el; does not compile (not even close :)
--
--  Next line is a typical skeleton placeholder; must be treated as a comment by the parser.

{header}

--
--  We expand each token in ada-skel-token-alist, after first deleting
--  the expected expansion. The result is checked by diff.
package body Ada_Skel is

   --  skeletons in ada-skel function alphabetical order, except split
   --  into declarations and statements

   --  ada-skel-record
   --EMACSCMD:(progn (forward-line 1)(kill-line 2)(insert "record record_type_1")(wisi-skel-expand))
   type Record_Type_1 is record
   end record;

   --  ada-skel-package-body
   --EMACSCMD:(progn (forward-line 1)(kill-line 3)(insert "package package_1")(let ((wisi-skel-test-input "body"))(wisi-skel-expand)))
   package body Package_1 is
   begin
   end Package_1;

   --  ada-skel-package-spec
   --EMACSCMD:(progn (forward-line 1)(kill-line 3)(insert "package package_1 ")(let ((wisi-skel-test-input "spec"))(wisi-skel-expand)))
   package Package_1 is
   private
   end Package_1;

   --  ada-skel-protected-body
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-line 2)(insert " Protected_1")(let ((wisi-skel-test-input "body"))(wisi-skel-expand)))
   protected body Protected_1 is
   end Protected_1;

   --  ada-skel-protected-spec
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-line 3)(insert " Protected_1")(let ((wisi-skel-test-input "spec"))(wisi-skel-expand)))
   protected type Protected_1 is
   private
   end Protected_1;

   --  ada-skel-task-body
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-line 4)(insert " Task_1")(let ((wisi-skel-test-input "body"))(wisi-skel-expand)))
   task body Task_1
   is
   begin
   end Task_1;

   --  ada-skel-task-spec
   --EMACSCMD:(progn (forward-line 1)(kill-line 3)(forward-char -1)(insert "\ntask Task_1")(let ((wisi-skel-test-input "spec"))(wisi-skel-expand)))
   task type Task_1 is
   end Task_1;

   --  ada-skel-return
   function Function_1 return Integer
   is begin
      --EMACSCMD:(progn (forward-line 1)(forward-word 1)(forward-char 1)(kill-line 2)(forward-char -1)(wisi-skel-expand))
      return  do
      end return;

   end Function_1;

   --  ada-skel-entry
   protected body Protected_1 is

      --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-line 4)(insert " Entry_1")(wisi-skel-expand))
      entry Entry_1 when
      is
      begin
      end Entry_1;

   end Protected_1;

begin

   --  ada-skel-accept
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-line 2)(insert " Accept_1")(wisi-skel-expand))
   accept Accept_1 do
   end Accept_1;

   --  ada-skel-case
   --EMACSCMD:(progn (forward-line 1)(forward-word 2)(kill-line 3)(wisi-skel-expand))
   case A is
      when =>
   end case;

   --  ada-skel-declare with block name
   --EMACSCMD:(progn (forward-line 1)(kill-line 5)(insert " declare Block_1")(wisi-skel-expand))
Block_1:
   declare
   begin
   exception
   end Block_1;

   --  ada-skel-declare without block name
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-line 4)(wisi-skel-expand))
   declare
   begin
   exception
   end;

   --  ada-skel-for with name
   --EMACSCMD:(progn (forward-line 1)(kill-line 3)(insert " for Loop_1")(wisi-skel-expand))
Loop_1 :
   for  loop
   end loop Loop_1;

   --  ada-skel-for without name
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-line 2)(wisi-skel-expand))
   for  loop
   end loop;

   --  ada-skel-if
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-line 4)(wisi-skel-expand))
   if  then
   elsif  then
   else
   end if;

   --  ada-skel-loop with name
   --EMACSCMD:(progn (forward-line 1)(kill-line 4)(insert "loop Loop_2")(wisi-skel-expand))
Loop_2 :
   loop
      exit Loop_2 when ;
   end loop Loop_2;

   --  ada-skel-select
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-line 3)(wisi-skel-expand))
   select
   else
   end select;

   --  ada-skel-while with name
   --EMACSCMD:(progn (forward-line 1)(kill-line 3)(insert " while Loop_3")(wisi-skel-expand))
Loop_3:
   while  loop
   end loop Loop_3;

   --  ada-skel-while without name
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(kill-line 2)(wisi-skel-expand))
   while  loop
   end loop;

end Ada_Skel;
