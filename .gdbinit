set args parse_partial none ../test/debug.adb --verbosity "debug=1 parse=1 mckenzie=1"

# catch excep
# catch excep ADA.ASSERTIONS.ASSERTION_ERROR
# catch excep WISITOKEN.SYNTAX_ERROR
# catch excep CONSTRAINT_ERROR
# catch excep SAL.PROGRAMMER_ERROR
# catch excep SAL.NOT_IMPLEMENTED
set max-value-size unlimited

# token_id
define show_id
  print ada_annex_p_actions.descriptor.image ($arg0).all
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
