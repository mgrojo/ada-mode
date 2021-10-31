# test_all_harness
#set args test_edit_source.adb DOS_Line_Endings "test=2"

# run_*_parse
#set args parse_partial indent test/debug.adb --verbosity "mckenzie=3" --mckenzie_task_count 1

# run_*_parse command_file
set args command_file debug.cmd

# Process_Command verbosity
break run_wisi_common_parse.adb:574 

# gpr_query
#set args -P test/ada_mode.gpr --db=c:/tmp/gpr_query.db --tracefile=gpr_query.trace

#catch excep
catch excep ADA.ASSERTIONS.ASSERTION_ERROR
catch excep WISITOKEN.SYNTAX_ERROR
catch excep CONSTRAINT_ERROR
catch excep SAL.PROGRAMMER_ERROR
catch excep SAL.NOT_IMPLEMENTED
catch except WisiToken.Parse.LR.McKenzie_Recover.Bad_Config
# catch except WisiToken.Parse.LR.McKenzie_Recover.Invalid_Case
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
  show_node $arg0.cur.ptr.element.node
end

# stream_node_ref
define show_ref
  p $arg0.stream.cur.ptr.element.label
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
