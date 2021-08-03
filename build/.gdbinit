# *_run.exe:
#set args --verbosity "debug=1" ../test/bnf/java_expressions_ch19.input

# wisitoken-bnf-generate:
#set args --test_main --output_bnf --verbosity "debug=1" ../test/bnf/skip_to_grammar.wy
#set args c:/Projects/org.emacs.ada-mode.stephe-6/gpr.wy

# t_mck:
#set args LALR "Error_2" "debug=1 test=1 mckenzie=2"
#catch except WisiToken.Parse.LR.McKenzie_Recover.Bad_Config

# t_one, t_all:
set args test_incremental.adb Lexer_Errors_1 "test=1"

#catch excep
catch excep SYSTEM.ASSERTIONS.ASSERT_FAILURE
catch excep WISITOKEN.SYNTAX_ERROR
catch excep CONSTRAINT_ERROR
catch excep SAL.PROGRAMMER_ERROR
catch excep SAL.NOT_IMPLEMENTED
set varsize-limit 0

# token_id
define show_id
#  print java_expressions_ch19_actions.descriptor.image ($arg0).all
#  print wisitoken_grammar_actions.descriptor.image ($arg0).all
  print ada_lite_actions.descriptor.image ($arg0).all
end

# node
define show_node 
  print $arg0.all
#  print java_expressions_ch19_actions.descriptor.image ($arg0.id).all
#  print wisitoken_grammar_actions.descriptor.image ($arg0.id).all
  print ada_lite_actions.descriptor.image ($arg0.id).all
end

define show_children
  print $arg0.shared_tree.nodes.elements ($arg1).children.elements.all ($arg0.shared_tree.nodes.elements ($arg1).children.first .. $arg0.shared_tree.nodes.elements ($arg1).children.last)
end

# stream_index
define show_element
  p $arg0.cur.ptr.element.label
  show_node $arg0.cur.ptr.element.node
end

# stream_node_ref
define show_ref
  show_element $arg0.element
  show_node    $arg0.node
end

define show_recover
  show_node $arg0.element_node
  show_node $arg0.node
end

# unbounded_string_object
define show_unbounded 
  print $arg0.reference.data (1 .. $arg0.reference.last)
end
