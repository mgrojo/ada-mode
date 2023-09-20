# *_run.exe:
#set args --verbosity "debug=1 parse=3" ../test/bnf/debug.input

# wisitoken-bnf-generate:
cd /Projects/org.emacs.gpr-mode/
set args --output_bnf gpr.wy 

# t_mck:
#set args LALR Missing_Name_2 "debug=1 mckenzie=1"
#catch except WisiToken.Parse.LR.McKenzie_Recover.Bad_Config
#catch except WisiToken.Parse.LR.McKenzie_Recover.Invalid_Case

# t_one, t_all:
#set args test_syntax_trees.adb Prev_New_Line_01 "debug=1 test=2 lexer=1"

catch excep
# catch excep ADA.ASSERTIONS.ASSERTION_ERROR
# catch excep WISITOKEN.SYNTAX_ERROR
# catch excep CONSTRAINT_ERROR
# catch excep SAL.PROGRAMMER_ERROR
# catch excep SAL.NOT_IMPLEMENTED
set max-value-size unlimited

# token_id
define show_id
#  print ada_lite_actions.descriptor.image ($arg0).all
#  print grammar_grammar_01_actions.descriptor.image ($arg0).all
  print Wisitoken_Grammar_Actions.descriptor.image ($arg0).all
end

# node
define show_node 
  print $arg0.all
  show_id $arg0.id
end

define show_children
  print $arg0.shared_tree.nodes.elements ($arg1).children.elements.all ($arg0.shared_tree.nodes.elements ($arg1).children.first .. $arg0.shared_tree.nodes.elements ($arg1).children.last)
end

# stream_index
define show_element
  show_node $arg0.cur.ptr.element.node
end

# stream curosor
define show_cur
  show_node $arg0.ptr.element.node
end

# stream_node_ref
define show_ref
  p $arg0.stream.cur.ptr.element.label
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
