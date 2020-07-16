// Function to run the tree-sitter parser, given a language object and input file name.
//
//  Copyright (C) 2009, 2010, 2013 - 2015, 2017 - 2020 Free Software Foundation, Inc.
//
//  This file is part of the WisiToken package.
//
//  This library is free software;  you can redistribute it and/or modify it
//  under terms of the  GNU General Public License  as published by the Free
//  Software  Foundation;  either version 3,  or (at your  option) any later
//  version. This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
//  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#include <stdio.h>
#include "tree_sitter/api.h"
#include "tree_sitter/parser.h"

int trace_parse = 0;

void print_log (void *payload, TSLogType type, const char * msg)
{
  if ((trace_parse >= 1 && type == TSLogTypeParse) ||
      (trace_parse >= 4 && type == TSLogTypeLex))
    fprintf (stdout, "%s\n", msg);
};

int wisitoken_tree_sitter_parse_file (const TSLanguage *language, const char* file_name, int trace_parse_1)
{
  FILE* input_file = fopen (file_name, "r");

  if (input_file == NULL)
    return errno;

  // We only use this on WisiToken test input files; they are all small
  const size_t buffer_size = 4096;
  char buffer[buffer_size];
  const size_t last = fread ((void *)buffer, 1, buffer_size, input_file);

  TSParser *parser = ts_parser_new();
  TSLogger logger = {NULL, print_log};

  trace_parse = trace_parse_1;

  ts_parser_set_language(parser, language);

  ts_parser_set_logger (parser, logger);

  TSTree *tree = ts_parser_parse_string (parser, NULL, buffer, last);

  // FIXME: output parse errors? return non-zero status
  // ts_node_has_error; traverse tree to find these?
  // ts_tree_root_node ts_node_parent, ts_node_child_count, ts_node_child
  //
  // or ts_tree_cursor_new

  // This doesn't work reliably on mingw, even after attempting to patch subtree.c
  /* TSNode root_node = ts_tree_root_node(tree); */
  /* char *string = ts_node_string(root_node); */
  /* printf("Syntax tree: %s\n", string); */
  // free (string);

  ts_tree_delete(tree);
  ts_parser_delete(parser);

  return 0;
}
// end of file
