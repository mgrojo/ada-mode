(defconst tokens
  [ABS
   ACCEPT
      ABORT
      ABSTRACT
      ACCESS
      ALIASED
      ALL
      AND
      ARRAY
      AT
      BEGIN
      BODY
      CASE
      CONSTANT
      DECLARE
      DELAY
      DELTA
      DIGITS
      DO
      ELSE
      ELSIF
      END
      ENTRY
      EXCEPTION
      EXIT
      FOR
      FUNCTION
      GENERIC
      GOTO
      IF
      IN
      INTERFACE
      IS
      LEFT_PAREN
      LIMITED
      LOOP
      MOD
      NEW
      NOT
      NULL
      OF
      OR
      OTHERS
      OUT
      OVERRIDING
      PACKAGE
      PRAGMA
      PRIVATE
      PROCEDURE
      PROTECTED
      RAISE
      RANGE
      RECORD
      REM
      RENAMES
      REQUEUE
      RETURN
      REVERSE
      RIGHT_PAREN
      SEPARATE
      SELECT
      SOME
      SUBTYPE
      SYNCHRONIZED
      TAGGED
      TASK
      TERMINATE
      THEN
      TYPE
      UNTIL
      USE
      WHEN
      WHILE
      WITH
      XOR
      AMPERSAND
      BAR
      BOX
      COLON
      COLON_EQUAL
      COMMA
      DOT
      DOT_DOT
      EQUAL
      EQUAL_GREATER
      GREATER
      GREATER_EQUAL
      GREATER_GREATER
      LESS
      LESS_EQUAL
      LESS_LESS
      MINUS
      PLUS
      SEMICOLON
      SLASH
      SLASH_EQUAL
      STAR
      STAR_STAR
      TICK
      IDENTIFIER
      STRING_LITERAL
      CHARACTER_LITERAL
      EOF

      opentoken_accept
      abstract_limited_synchronized_opt
      abstract_limited_opt
      abstract_tagged_limited_opt
      abstract_subprogram_declaration
      accept_statement
      access_definition
      actual_parameter_part
      actual_parameter_part_opt
      aggregate
      aliased_opt
      and_interface_list_opt
      array_type_definition
      aspect_clause
      aspect_specification_opt
      assignment_statement
      association_opt
      association_list
      asynchronous_select
      at_clause
      attribute_reference
      attribute_designator
      binary_adding_operator
      block_statement
      body_g
      body_stub
      case_expression
      case_expression_alternative
      case_expression_alternative_list
      case_statement
      case_statement_alternative
      case_statement_alternative_list
      choice_expression
      choice_relation_and_list
      choice_relation_or_list
      choice_relation_xor_list
      choice_relation_and_then_list
      choice_relation_or_else_list
      choice_relation
      compilation_unit
      compilation_unit_list
      component_clause
      component_clause_list
      component_declaration
      component_definition
      component_item
      component_list
      component_list_opt
      compound_statement
      conditional_entry_call
      constant_opt
      constraint
      constraint_opt
      context_item
      declaration
      declarations
      declarative_part_opt
      delay_alternative
      delay_statement
      derived_type_definition
      direct_name
      direct_name_opt
      discrete_choice
      discrete_choice_list
      discrete_subtype_definition
      discrete_subtype_definition_list
      discriminant_part_opt
      discriminant_specification_opt
      discriminant_specification_list
      elsif_expression_item
      elsif_expression_list
      elsif_statement_item
      elsif_statement_list
      entry_body
      entry_body_formal_part
      entry_call_alternative
      entry_declaration
      enumeration_literal
      enumeration_literal_list
      enumeration_representation_clause
      enumeration_type_definition
      exception_choice
      exception_choice_list
      exception_declaration
      exception_handler
      exception_handler_list
      exception_handler_list_opt
      exit_statement
      expression
      expression_opt
      expression_function_declaration
      extended_return_object_declaration
      extended_return_object_declaration_opt
      extended_return_statement
      factor
      formal_object_declaration
      formal_part
      formal_subprogram_declaration
      formal_type_declaration
      formal_type_definition
      formal_derived_type_definition
      formal_package_declaration
      formal_package_actual_part
      full_type_declaration
      function_specification
      general_access_modifier_opt
      generic_declaration
      generic_formal_part
      generic_formal_parameter_declarations
      generic_formal_parameter_declaration
      generic_instantiation
      generic_package_declaration
      generic_renaming_declaration
      generic_subprogram_declaration
      handled_sequence_of_statements
      identifier_list
      identifier_opt
      if_expression
      if_statement
      incomplete_type_declaration
      index_constraint
      index_subtype_definition
      index_subtype_definition_list
      interface_list
      interface_type_definition
      iteration_scheme
      iterator_specification
      iterator_specification_opt
      label_opt
      library_item
      library_unit_declaration
      library_unit_renaming_declaration
      loop_statement
      membership_choice_list
      membership_choice
      mod_clause_opt
      mode_opt
      multiplying_operator
      name_list
      name
      name_opt
      null_exclusion_opt
      null_exclusion_opt_name
      null_procedure_declaration
      object_declaration
      object_renaming_declaration
      overriding_indicator_opt
      package_body
      package_body_stub
      package_declaration
      package_renaming_declaration
      package_specification
      parameter_and_result_profile
      parameter_profile_opt
      parameter_specification
      parameter_specification_list
      paren_expression
      pragma_g
      pragma_argument_association
      pragma_argument_association_list
      primary
      private_extension_declaration
      private_type_declaration
      procedure_call_statement
      procedure_specification
      proper_body
      protected_body
      protected_body_stub
      protected_definition
      protected_operation_item
      protected_operation_item_list
      protected_operation_item_list_opt
      protected_opt
      protected_type_declaration
      qualified_expression
      quantified_expression
      quantifier
      raise_statement
      range_g
      real_range_specification_opt
      record_definition
      record_representation_clause
      record_rep
      record_type_definition
      relation_and_list
      relation_and_then_list
      relation_or_list
      relation_or_else_list
      relation_xor_list
      relation
      relational_operator
      renaming_declaration
      requeue_statement
      return_subtype_indication
      selected_component
      selective_accept
      select_alternative
      select_alternative_list
      select_alternative_list_opt
      select_statement
      sequence_of_statements
      sequence_of_statements_opt
      simple_expression
      simple_return_statement
      simple_statement
      single_protected_declaration
      single_task_declaration
      statement
      subprogram_body
      subprogram_body_stub
      subprogram_declaration
      subprogram_default
      subprogram_renaming_declaration
      subprogram_specification
      subtype_declaration
      subtype_indication
      subunit
      task_body
      task_body_stub
      task_definition
      task_type_declaration
      term
      term_list
      timed_entry_call
      triggering_alternative
      type_declaration
      type_definition
      variant_part
      variant_list
      variant
      unary_adding_operator
      use_clause
      with_clause
      ])

(defconst classes
  [block-end
    block-middle ;; not start of statement
    block-start ;; start of block is start of statement
    close-paren
    keyword    ;; cached only for face; not used in indentation
    list-break
    name
    name-paren ;; anything that looks like a procedure call, since the grammar can't distinguish most of them
    open-paren
    return
    return-1
    return-2
    statement-end
    statement-other
    statement-start
    type
   ])

(defun tokens-index (token)
  (let ((result 0))
    (condition-case nil
	(while (not (equal token (aref tokens result)))
	  (setq result (1+ result)))
      (error
       (error "%s not found in tokens" token)))
    result))

(defun classes-index (term)
  (let ((result 0))
    (while (not (equal term (aref classes result)))
      (setq result (1+ result)))
    result))

(defun extract-text-props ()
  "Extract all 'wisi-cache text properties from current buffer,
build sexps to set them in \"*wisi-cache*\"."
  (interactive)
  (let ((source-buffer (current-buffer))
	(sexp-buffer (get-buffer-create "*wisi-cache*"))
	(pos (point-min))
	cache
	(cache-count 0))

    (set-buffer sexp-buffer)
    (erase-buffer)
    (set-buffer source-buffer)

    (goto-char (point-min))
    (while (or (get-text-property pos 'wisi-cache)
	       (setq pos (next-single-property-change (point) 'wisi-cache)))
      (setq cache-count (1+ cache-count))
      (setq cache (get-text-property pos 'wisi-cache))
      (set-buffer sexp-buffer)
      ;; FIXME: first cut, just nil for containing, prev, next, end markers
      (insert (format "%6d %3d %3d %2d %2d\n"
		      pos
		      (tokens-index (wisi-cache-nonterm cache))
		      (tokens-index (wisi-cache-token cache))
		      (wisi-cache-last cache)
		      (classes-index (wisi-cache-class cache))))

      (set-buffer source-buffer)
      (setq pos (1+ pos))
      (goto-char pos)
      )
    (message "%d caches" cache-count)
    ))

(defun apply-text-props ()
  "In current buffer, apply contents of \"*wisi-cache*\"."
  (interactive)
  (let ((source-buffer (current-buffer))
	(data-buffer (get-buffer "*wisi-cache*"))
	line-begin
	pos nonterm token last class)

    (set-buffer data-buffer)
    (goto-char (point-min))

    (while (not (eobp))
      (setq line-begin (line-beginning-position))

      (setq pos (string-to-number (buffer-substring-no-properties line-begin (+ line-begin 6))))

      (setq nonterm (aref tokens (string-to-number (buffer-substring-no-properties
						    (+ line-begin 7) (+ line-begin 10)))))

      (setq token (aref tokens (string-to-number (buffer-substring-no-properties
						  (+ line-begin 11) (+ line-begin 14)))))

      (setq last (string-to-number (buffer-substring-no-properties
				    (+ line-begin 15) (+ line-begin 17))))

      (setq class (aref classes (string-to-number (buffer-substring-no-properties
						   (+ line-begin 18) (+ line-begin 20)))))

      (forward-line 1)

      (put-text-property
       pos (1+ pos)
       'wisi-cache
       (wisi-cache-create
	:nonterm    nonterm
	:token      token
	:last       last
	:class      class)
       source-buffer)
      )

    (set-buffer source-buffer)
    ))

(defun pipe-torture (exe &rest args)
  ""
  (let ((proc (apply 'start-process "pipe-reader" "pipe-read" exe args))
	start-time)

    (unless (process-live-p proc)
      (error "process died"))

    (setq start-time (time-to-seconds (current-time)))
    (process-send-region proc (point-min) (point-max))
    (message "send time %f" (- (time-to-seconds (current-time)) start-time))
    ))

(defun pipe-torture-read (file)
  ""
  (let ((proc (apply 'start-process "pipe-reader" "pipe-read" (list "bash" "-c" (concat "cat < " file))))
	start-time)

    (unless (process-live-p proc)
      (error "process died"))

    (setq start-time (time-to-seconds (current-time)))
    (while (process-live-p proc)
      (accept-process-output proc))

    (message "read time %f" (- (time-to-seconds (current-time)) start-time))
    ))

(provide 'extract-text-props) ;; allow using byte-compiled versions of code.
