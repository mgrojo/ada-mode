set args parse indent test/debug.wy --debug_mode --task_count 1 --verbosity 1 1 0

catch exception
set varsize-limit 0

# token_id
define show_id
  print wisitoken_grammar_1_process_actions.descriptor.image ($arg0).all
end

# node
define show_node 
  print $arg0.all
end

define show_children
  print $arg0.children.elements.all
end

# unbounded_string_object
define show_unbounded 
  print $arg0.reference.data (1 .. $arg0.reference.last)
end
# end of file
