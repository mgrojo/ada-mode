# test_all_harness
#set args test_edit_source.adb DOS_Line_Endings "test=2"

# run_*_parse
set args parse_partial indent test/debug.adb --verbosity "action=3" 

# run_*_parse command_file
#set args command_file debug.cmd 

# gpr_query
#set args -P test/ada_mode.gpr --db=c:/tmp/gpr_query.db --tracefile=gpr_query.trace

#catch excep
#catch excep SYSTEM.ASSERTIONS.ASSERT_FAILURE
catch excep WISITOKEN.SYNTAX_ERROR
catch excep CONSTRAINT_ERROR
catch excep SAL.PROGRAMMER_ERROR
catch excep SAL.NOT_IMPLEMENTED
#catch excep WISITOKEN.PARSE.LR.MCKENZIE_RECOVER.BAD_CONFIG
set varsize-limit 0

# token_id
define show_id
  print ada_annex_p_process_actions.descriptor.image ($arg0).all
#  print gpr_process_actions.descriptor.image ($arg0).all
end

# node_access
define show_node 
  print $arg0.all
  print ada_annex_p_process_actions.descriptor.image ($.id).all
#  print gpr_process_actions.descriptor.image ($.id).all
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

# recover_token
define show_recover
  show_node $arg0.element_node
  show_node $arg0.node
end

# unbounded_string_object
define show_unbounded 
  print $arg0.reference.data (1 .. $arg0.reference.last)
end

set print thread-events off
# end of file
