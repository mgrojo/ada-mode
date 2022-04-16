set args command_file debug.cmd

catch exception
set varsize-limit 0

# token_id
define show_id
  print wisitoken_grammar_1_process_actions.descriptor.image ($arg0).all
end

# node
define show_node 
  print $arg0.all
  show_id $arg0.id
end

define show_children
  print $arg0.children.elements.all
end

# stream_index
define show_element
  show_node $arg0.cur.ptr.element.node
end

# stream_node_ref
define show_ref
  p $arg0.stream.cur.ptr.element.label
  show_element $arg0.element
  show_node    $arg0.node
end

# unbounded_string_object
define show_unbounded 
  print $arg0.reference.data (1 .. $arg0.reference.last)
end
# end of file
