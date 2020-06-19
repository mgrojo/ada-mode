#ifndef WISITOKEN_TREE_SITTER_PARSER_H_
#define WISITOKEN_TREE_SITTER_PARSER_H_

#include "tree_sitter/parser.h"

int wisitoken_tree_sitter_parse_file (const TSLanguage *language, const char* file_name);

#endif  // WISITOKEN_TREE_SITTER_PARSER_H_
