import * as Blockly from 'blockly';

export const cobolGenerator = new Blockly.Generator('COBOL');

cobolGenerator.scrub_ = (block, code) => code;

// Define required order constants
cobolGenerator.ORDER_ATOMIC = 0;
cobolGenerator.ORDER_NONE = 99;

// =============================================
// IDENTIFICATION DIVISION GENERATORS
// =============================================

cobolGenerator['cobol_identification_division'] = block => {
  const prog = block.getFieldValue('PROG');
  const author = block.getFieldValue('AUTHOR');
  const installation = block.getFieldValue('INSTALLATION');
  const written = block.getFieldValue('WRITTEN');
  const compiled = block.getFieldValue('COMPILED');
  const security = block.getFieldValue('SECURITY');
  
  return `IDENTIFICATION DIVISION.
PROGRAM-ID. ${prog}.
AUTHOR. ${author}.
INSTALLATION. ${installation}.
DATE-WRITTEN. ${written}.
DATE-COMPILED. ${compiled}.
SECURITY. ${security}.
`;
};

cobolGenerator['cobol_remarks'] = block => {
  const remarks = block.getFieldValue('REMARKS');
  return `REMARKS. ${remarks}.\n`;
};

// =============================================
// ENVIRONMENT DIVISION GENERATORS
// =============================================

cobolGenerator['cobol_environment_division'] = () => 
  `ENVIRONMENT DIVISION.\n`;

cobolGenerator['cobol_configuration_section'] = block => {
  const source = block.getFieldValue('SOURCE');
  const object = block.getFieldValue('OBJECT');
  const special = block.getFieldValue('SPECIAL');
  
  return `CONFIGURATION SECTION.
SOURCE-COMPUTER. ${source}.
OBJECT-COMPUTER. ${object}.
SPECIAL-NAMES. ${special}.
`;
};

cobolGenerator['cobol_input_output_section'] = () =>
  `INPUT-OUTPUT SECTION.
FILE-CONTROL.
`;

cobolGenerator['cobol_select_file'] = block => {
  const select = block.getFieldValue('SELECT');
  const assign = block.getFieldValue('ASSIGN');
  const org = block.getFieldValue('ORG');
  const access = block.getFieldValue('ACCESS');
  
  return `SELECT ${select}
    ASSIGN TO ${assign}
    ORGANIZATION IS ${org}
    ACCESS MODE IS ${access}.
`;
};

// =============================================
// DATA DIVISION GENERATORS
// =============================================

cobolGenerator['cobol_data_division'] = () =>
  `DATA DIVISION.\n`;

cobolGenerator['cobol_file_section'] = () =>
  `FILE SECTION.\n`;

cobolGenerator['cobol_fd_record'] = block => {
  const fd = block.getFieldValue('FD');
  const label = block.getFieldValue('LABEL');
  const size = block.getFieldValue('SIZE');
  const record = block.getFieldValue('RECORD');
  
  return `FD ${fd}
    LABEL RECORDS ARE ${label}
    RECORD CONTAINS ${size} CHARACTERS.
01 ${record}.
`;
};

cobolGenerator['cobol_working_storage_section'] = () =>
  `WORKING-STORAGE SECTION.\n`;

cobolGenerator['cobol_variable_declaration'] = block => {
  const level = block.getFieldValue('LEVEL');
  const varName = block.getFieldValue('VAR');
  const pic = block.getFieldValue('PIC');
  const value = block.getFieldValue('VALUE');
  
  let code = `${level} ${varName} PIC ${pic}`;
  if (value && value.trim() !== '') {
    code += ` VALUE ${value}`;
  }
  return code + '.\n';
};

cobolGenerator['cobol_group_item'] = block => {
  const level = block.getFieldValue('LEVEL');
  const name = block.getFieldValue('NAME');
  const items = cobolGenerator.statementToCode(block, 'ITEMS');
  
  return `${level} ${name}.\n${items}`;
};

cobolGenerator['cobol_occurs_clause'] = block => {
  const level = block.getFieldValue('LEVEL');
  const name = block.getFieldValue('NAME');
  const times = block.getFieldValue('TIMES');
  const pic = block.getFieldValue('PIC');
  
  return `${level} ${name} OCCURS ${times} TIMES PIC ${pic}.\n`;
};

cobolGenerator['cobol_linkage_section'] = () =>
  `LINKAGE SECTION.\n`;

// =============================================
// PROCEDURE DIVISION GENERATORS
// =============================================

cobolGenerator['cobol_procedure_division'] = () =>
  `PROCEDURE DIVISION.\n`;

cobolGenerator['cobol_procedure_using'] = block => {
  const parms = block.getFieldValue('PARMS');
  return `PROCEDURE DIVISION USING ${parms}.\n`;
};

cobolGenerator['cobol_paragraph'] = block => {
  const paraName = block.getFieldValue('PARA');
  const body = cobolGenerator.statementToCode(block, 'BODY');
  return `${paraName}.\n${body}`;
};

cobolGenerator['cobol_section'] = block => {
  const sectionName = block.getFieldValue('SECTION');
  const body = cobolGenerator.statementToCode(block, 'BODY');
  return `${sectionName} SECTION.\n${body}`;
};

// =============================================
// INPUT/OUTPUT GENERATORS
// =============================================

cobolGenerator['cobol_display'] = block => {
  const text = block.getFieldValue('TEXT');
  return `DISPLAY "${text}".\n`;
};

cobolGenerator['cobol_display_variable'] = block => {
  const varName = block.getFieldValue('VAR');
  return `DISPLAY ${varName}.\n`;
};

cobolGenerator['cobol_accept'] = block => {
  const varName = block.getFieldValue('VAR');
  return `ACCEPT ${varName}.\n`;
};

cobolGenerator['cobol_accept_date'] = block => {
  const varName = block.getFieldValue('VAR');
  const source = block.getFieldValue('SOURCE');
  return `ACCEPT ${varName} FROM ${source}.\n`;
};

// =============================================
// FILE OPERATION GENERATORS
// =============================================

cobolGenerator['cobol_open'] = block => {
  const mode = block.getFieldValue('MODE');
  const file = block.getFieldValue('FILE');
  return `OPEN ${mode} ${file}.\n`;
};

cobolGenerator['cobol_close'] = block => {
  const file = block.getFieldValue('FILE');
  return `CLOSE ${file}.\n`;
};

cobolGenerator['cobol_read'] = block => {
  const file = block.getFieldValue('FILE');
  const endAction = block.getFieldValue('END_ACTION');
  return `READ ${file}
    AT END ${endAction}.\n`;
};

cobolGenerator['cobol_write'] = block => {
  const record = block.getFieldValue('RECORD');
  return `WRITE ${record}.\n`;
};

cobolGenerator['cobol_rewrite'] = block => {
  const record = block.getFieldValue('RECORD');
  return `REWRITE ${record}.\n`;
};

cobolGenerator['cobol_delete'] = block => {
  const file = block.getFieldValue('FILE');
  return `DELETE ${file}.\n`;
};

// =============================================
// ARITHMETIC GENERATORS
// =============================================

cobolGenerator['cobol_move'] = block => {
  const val = block.getFieldValue('VAL');
  const varName = block.getFieldValue('VAR');
  return `MOVE ${val} TO ${varName}.\n`;
};

cobolGenerator['cobol_add'] = block => {
  const num = block.getFieldValue('NUM');
  const varName = block.getFieldValue('VAR');
  return `ADD ${num} TO ${varName}.\n`;
};

cobolGenerator['cobol_add_giving'] = block => {
  const a = block.getFieldValue('A');
  const b = block.getFieldValue('B');
  const result = block.getFieldValue('RESULT');
  return `ADD ${a} TO ${b} GIVING ${result}.\n`;
};

cobolGenerator['cobol_subtract'] = block => {
  const num = block.getFieldValue('NUM');
  const varName = block.getFieldValue('VAR');
  return `SUBTRACT ${num} FROM ${varName}.\n`;
};

cobolGenerator['cobol_subtract_giving'] = block => {
  const a = block.getFieldValue('A');
  const b = block.getFieldValue('B');
  const result = block.getFieldValue('RESULT');
  return `SUBTRACT ${a} FROM ${b} GIVING ${result}.\n`;
};

cobolGenerator['cobol_multiply'] = block => {
  const a = block.getFieldValue('A');
  const b = block.getFieldValue('B');
  return `MULTIPLY ${a} BY ${b}.\n`;
};

cobolGenerator['cobol_multiply_giving'] = block => {
  const a = block.getFieldValue('A');
  const b = block.getFieldValue('B');
  const result = block.getFieldValue('RESULT');
  return `MULTIPLY ${a} BY ${b} GIVING ${result}.\n`;
};

cobolGenerator['cobol_divide'] = block => {
  const a = block.getFieldValue('A');
  const b = block.getFieldValue('B');
  return `DIVIDE ${a} BY ${b}.\n`;
};

cobolGenerator['cobol_divide_giving'] = block => {
  const a = block.getFieldValue('A');
  const b = block.getFieldValue('B');
  const result = block.getFieldValue('RESULT');
  const remainder = block.getFieldValue('REMAINDER');
  return `DIVIDE ${a} BY ${b} GIVING ${result} REMAINDER ${remainder}.\n`;
};

cobolGenerator['cobol_compute'] = block => {
  const result = block.getFieldValue('RESULT');
  const expression = block.getFieldValue('EXPRESSION');
  return `COMPUTE ${result} = ${expression}.\n`;
};

// =============================================
// CONTROL FLOW GENERATORS
// =============================================

cobolGenerator['cobol_if'] = block => {
  const cond = cobolGenerator.valueToCode(block, 'COND', cobolGenerator.ORDER_NONE) || 'CONDITION';
  const thenBranch = cobolGenerator.statementToCode(block, 'THEN');
  const elseBranch = cobolGenerator.statementToCode(block, 'ELSE');
  
  let code = `IF ${cond} THEN\n${thenBranch}`;
  if (elseBranch.trim() !== '') {
    code += `ELSE\n${elseBranch}`;
  }
  return code + 'END-IF.\n';
};

cobolGenerator['cobol_condition'] = block => {
  const left = block.getFieldValue('LEFT');
  const op = block.getFieldValue('OP');
  const right = block.getFieldValue('RIGHT');
  const code = `${left} ${op} ${right}`;
  return [code, cobolGenerator.ORDER_ATOMIC];
};

cobolGenerator['cobol_evaluate'] = block => {
  const val = cobolGenerator.valueToCode(block, 'VALUE', cobolGenerator.ORDER_NONE) || 'UNKNOWN';
  const body = cobolGenerator.statementToCode(block, 'WHEN');
  return `EVALUATE ${val}\n${body}END-EVALUATE.\n`;
};

cobolGenerator['cobol_when_case'] = block => {
  const val = block.getFieldValue('VALUE');
  const body = cobolGenerator.statementToCode(block, 'DO');
  return `  WHEN ${val}\n${body}`;
};

cobolGenerator['cobol_when_other'] = block => {
  const body = cobolGenerator.statementToCode(block, 'DO');
  return `  WHEN OTHER\n${body}`;
};

// =============================================
// LOOP GENERATORS
// =============================================

cobolGenerator['cobol_perform'] = block => {
  const para = block.getFieldValue('PARA');
  return `PERFORM ${para}.\n`;
};

cobolGenerator['cobol_perform_times'] = block => {
  const para = block.getFieldValue('PARA');
  const times = block.getFieldValue('TIMES');
  return `PERFORM ${para} ${times} TIMES.\n`;
};

cobolGenerator['cobol_perform_until'] = block => {
  const cond = cobolGenerator.valueToCode(block, 'COND', cobolGenerator.ORDER_NONE);
  const body = cobolGenerator.statementToCode(block, 'DO');
  return `PERFORM UNTIL ${cond}\n${body}END-PERFORM.\n`;
};

cobolGenerator['cobol_perform_varying'] = block => {
  const varName = block.getFieldValue('VAR');
  const from = block.getFieldValue('FROM');
  const by = block.getFieldValue('BY');
  const until = block.getFieldValue('UNTIL');
  const body = cobolGenerator.statementToCode(block, 'DO');
  
  return `PERFORM VARYING ${varName} FROM ${from} BY ${by} UNTIL ${until}\n${body}END-PERFORM.\n`;
};

cobolGenerator['cobol_goto'] = block => {
  const para = block.getFieldValue('PARA');
  return `GO TO ${para}.\n`;
};

// =============================================
// STRING MANIPULATION GENERATORS
// =============================================

cobolGenerator['cobol_string_concatenate'] = block => {
  const var1 = block.getFieldValue('VAR1');
  const delim1 = block.getFieldValue('DELIM1');
  const var2 = block.getFieldValue('VAR2');
  const delim2 = block.getFieldValue('DELIM2');
  const result = block.getFieldValue('RESULT');
  
  return `STRING ${var1} DELIMITED BY ${delim1}
    ${var2} DELIMITED BY ${delim2}
    INTO ${result}.\n`;
};

cobolGenerator['cobol_unstring'] = block => {
  const source = block.getFieldValue('SOURCE');
  const delim = block.getFieldValue('DELIM');
  const vars = block.getFieldValue('VARS');
  
  return `UNSTRING ${source} DELIMITED BY ${delim}
    INTO ${vars}.\n`;
};

cobolGenerator['cobol_inspect'] = block => {
  const varName = block.getFieldValue('VAR');
  const action = block.getFieldValue('ACTION');
  const forClause = block.getFieldValue('FOR');
  const char = block.getFieldValue('CHAR');
  
  return `INSPECT ${varName} ${action} ${forClause} ${char}.\n`;
};

// =============================================
// INITIALIZATION AND UTILITY GENERATORS
// =============================================

cobolGenerator['cobol_initialize'] = block => {
  const varName = block.getFieldValue('VAR');
  return `INITIALIZE ${varName}.\n`;
};

cobolGenerator['cobol_initialize_multiple'] = block => {
  const vars = block.getFieldValue('VARS');
  return `INITIALIZE ${vars}.\n`;
};

cobolGenerator['cobol_set'] = block => {
  const index = block.getFieldValue('INDEX');
  const value = block.getFieldValue('VALUE');
  return `SET ${index} TO ${value}.\n`;
};

// =============================================
// PROGRAM CONTROL GENERATORS
// =============================================

cobolGenerator['cobol_call'] = block => {
  const prog = block.getFieldValue('PROG');
  return `CALL ${prog}.\n`;
};

cobolGenerator['cobol_call_using'] = block => {
  const prog = block.getFieldValue('PROG');
  const parms = block.getFieldValue('PARMS');
  return `CALL ${prog} USING ${parms}.\n`;
};

cobolGenerator['cobol_exit'] = () =>
  `EXIT.\n`;

cobolGenerator['cobol_exit_program'] = () =>
  `EXIT PROGRAM.\n`;

cobolGenerator['cobol_stop_run'] = () =>
  `STOP RUN.\n`;

cobolGenerator['cobol_return'] = () =>
  `RETURN.\n`;

// =============================================
// ERROR HANDLING GENERATORS
// =============================================

cobolGenerator['cobol_on_size_error'] = block => {
  const errorHandler = cobolGenerator.statementToCode(block, 'ERROR_HANDLER');
  return `ON SIZE ERROR\n${errorHandler}`;
};

cobolGenerator['cobol_not_on_size_error'] = block => {
  const successHandler = cobolGenerator.statementToCode(block, 'SUCCESS_HANDLER');
  return `NOT ON SIZE ERROR\n${successHandler}`;
};

cobolGenerator['cobol_invalid_key'] = block => {
  const errorHandler = cobolGenerator.statementToCode(block, 'ERROR_HANDLER');
  return `INVALID KEY\n${errorHandler}`;
};

// =============================================
// SORT AND MERGE GENERATORS
// =============================================

cobolGenerator['cobol_sort'] = block => {
  const sortFile = block.getFieldValue('SORT_FILE');
  const order = block.getFieldValue('ORDER');
  const key = block.getFieldValue('KEY');
  const input = block.getFieldValue('INPUT');
  const output = block.getFieldValue('OUTPUT');
  
  return `SORT ${sortFile}
    ON ${order} ${key}
    USING ${input}
    GIVING ${output}.\n`;
};

cobolGenerator['cobol_merge'] = block => {
  const mergeFile = block.getFieldValue('MERGE_FILE');
  const order = block.getFieldValue('ORDER');
  const key = block.getFieldValue('KEY');
  const input = block.getFieldValue('INPUT');
  const output = block.getFieldValue('OUTPUT');
  
  return `MERGE ${mergeFile}
    ON ${order} ${key}
    USING ${input}
    GIVING ${output}.\n`;
};

// =============================================
// TABLE HANDLING GENERATORS
// =============================================

cobolGenerator['cobol_search'] = block => {
  const table = block.getFieldValue('TABLE');
  const atEnd = block.getFieldValue('AT_END');
  const when = block.getFieldValue('WHEN');
  const foundAction = cobolGenerator.statementToCode(block, 'FOUND_ACTION');
  
  return `SEARCH ${table}
    AT END ${atEnd}
    WHEN ${when}
${foundAction}END-SEARCH.\n`;
};

cobolGenerator['cobol_search_all'] = block => {
  const table = block.getFieldValue('TABLE');
  const atEnd = block.getFieldValue('AT_END');
  const when = block.getFieldValue('WHEN');
  const foundAction = cobolGenerator.statementToCode(block, 'FOUND_ACTION');
  
  return `SEARCH ALL ${table}
    AT END ${atEnd}
    WHEN ${when}
${foundAction}END-SEARCH.\n`;
};

// =============================================
// REPORT WRITER GENERATORS
// =============================================

cobolGenerator['cobol_report_section'] = block => {
  const report = block.getFieldValue('REPORT');
  return `REPORT SECTION.
RD ${report}.\n`;
};

cobolGenerator['cobol_initiate'] = block => {
  const report = block.getFieldValue('REPORT');
  return `INITIATE ${report}.\n`;
};

cobolGenerator['cobol_generate'] = block => {
  const line = block.getFieldValue('LINE');
  return `GENERATE ${line}.\n`;
};

cobolGenerator['cobol_terminate'] = block => {
  const report = block.getFieldValue('REPORT');
  return `TERMINATE ${report}.\n`;
};

// =============================================
// SPECIAL COBOL CONSTRUCTS GENERATORS
// =============================================

cobolGenerator['cobol_copy'] = block => {
  const copybook = block.getFieldValue('COPYBOOK');
  return `COPY ${copybook}.\n`;
};

cobolGenerator['cobol_replace'] = block => {
  const copybook = block.getFieldValue('COPYBOOK');
  const oldText = block.getFieldValue('OLD');
  const newText = block.getFieldValue('NEW');
  return `COPY ${copybook} REPLACING ${oldText} BY ${newText}.\n`;
};

cobolGenerator['cobol_exec_sql'] = block => {
  const sql = block.getFieldValue('SQL');
  return `EXEC SQL
    ${sql}
END-EXEC.\n`;
};

cobolGenerator['cobol_exec_cics'] = block => {
  const cics = block.getFieldValue('CICS');
  return `EXEC CICS
    ${cics}
END-EXEC.\n`;
};

// =============================================
// COMPILER DIRECTIVES GENERATORS
// =============================================

cobolGenerator['cobol_eject'] = () =>
  `EJECT.\n`;

cobolGenerator['cobol_skip'] = block => {
  const lines = block.getFieldValue('LINES');
  return `SKIP${lines}.\n`;
};

cobolGenerator['cobol_title'] = block => {
  const title = block.getFieldValue('TITLE');
  return `TITLE '${title}'.\n`;
};

// =============================================
// REGISTER ALL BLOCKS WITH forBlock
// =============================================

// Identification Division
cobolGenerator.forBlock['cobol_identification_division'] = cobolGenerator['cobol_identification_division'];
cobolGenerator.forBlock['cobol_remarks'] = cobolGenerator['cobol_remarks'];

// Environment Division
cobolGenerator.forBlock['cobol_environment_division'] = cobolGenerator['cobol_environment_division'];
cobolGenerator.forBlock['cobol_configuration_section'] = cobolGenerator['cobol_configuration_section'];
cobolGenerator.forBlock['cobol_input_output_section'] = cobolGenerator['cobol_input_output_section'];
cobolGenerator.forBlock['cobol_select_file'] = cobolGenerator['cobol_select_file'];

// Data Division
cobolGenerator.forBlock['cobol_data_division'] = cobolGenerator['cobol_data_division'];
cobolGenerator.forBlock['cobol_file_section'] = cobolGenerator['cobol_file_section'];
cobolGenerator.forBlock['cobol_fd_record'] = cobolGenerator['cobol_fd_record'];
cobolGenerator.forBlock['cobol_working_storage_section'] = cobolGenerator['cobol_working_storage_section'];
cobolGenerator.forBlock['cobol_variable_declaration'] = cobolGenerator['cobol_variable_declaration'];
cobolGenerator.forBlock['cobol_group_item'] = cobolGenerator['cobol_group_item'];
cobolGenerator.forBlock['cobol_occurs_clause'] = cobolGenerator['cobol_occurs_clause'];
cobolGenerator.forBlock['cobol_linkage_section'] = cobolGenerator['cobol_linkage_section'];

// Procedure Division
cobolGenerator.forBlock['cobol_procedure_division'] = cobolGenerator['cobol_procedure_division'];
cobolGenerator.forBlock['cobol_procedure_using'] = cobolGenerator['cobol_procedure_using'];
cobolGenerator.forBlock['cobol_paragraph'] = cobolGenerator['cobol_paragraph'];
cobolGenerator.forBlock['cobol_section'] = cobolGenerator['cobol_section'];

// Input/Output
cobolGenerator.forBlock['cobol_display'] = cobolGenerator['cobol_display'];
cobolGenerator.forBlock['cobol_display_variable'] = cobolGenerator['cobol_display_variable'];
cobolGenerator.forBlock['cobol_accept'] = cobolGenerator['cobol_accept'];
cobolGenerator.forBlock['cobol_accept_date'] = cobolGenerator['cobol_accept_date'];

// File Operations
cobolGenerator.forBlock['cobol_open'] = cobolGenerator['cobol_open'];
cobolGenerator.forBlock['cobol_close'] = cobolGenerator['cobol_close'];
cobolGenerator.forBlock['cobol_read'] = cobolGenerator['cobol_read'];
cobolGenerator.forBlock['cobol_write'] = cobolGenerator['cobol_write'];
cobolGenerator.forBlock['cobol_rewrite'] = cobolGenerator['cobol_rewrite'];
cobolGenerator.forBlock['cobol_delete'] = cobolGenerator['cobol_delete'];

// Arithmetic
cobolGenerator.forBlock['cobol_move'] = cobolGenerator['cobol_move'];
cobolGenerator.forBlock['cobol_add'] = cobolGenerator['cobol_add'];
cobolGenerator.forBlock['cobol_add_giving'] = cobolGenerator['cobol_add_giving'];
cobolGenerator.forBlock['cobol_subtract'] = cobolGenerator['cobol_subtract'];
cobolGenerator.forBlock['cobol_subtract_giving'] = cobolGenerator['cobol_subtract_giving'];
cobolGenerator.forBlock['cobol_multiply'] = cobolGenerator['cobol_multiply'];
cobolGenerator.forBlock['cobol_multiply_giving'] = cobolGenerator['cobol_multiply_giving'];
cobolGenerator.forBlock['cobol_divide'] = cobolGenerator['cobol_divide'];
cobolGenerator.forBlock['cobol_divide_giving'] = cobolGenerator['cobol_divide_giving'];
cobolGenerator.forBlock['cobol_compute'] = cobolGenerator['cobol_compute'];

// Control Flow
cobolGenerator.forBlock['cobol_if'] = cobolGenerator['cobol_if'];
cobolGenerator.forBlock['cobol_condition'] = cobolGenerator['cobol_condition'];
cobolGenerator.forBlock['cobol_evaluate'] = cobolGenerator['cobol_evaluate'];
cobolGenerator.forBlock['cobol_when_case'] = cobolGenerator['cobol_when_case'];
cobolGenerator.forBlock['cobol_when_other'] = cobolGenerator['cobol_when_other'];

// Loops
cobolGenerator.forBlock['cobol_perform'] = cobolGenerator['cobol_perform'];
cobolGenerator.forBlock['cobol_perform_times'] = cobolGenerator['cobol_perform_times'];
cobolGenerator.forBlock['cobol_perform_until'] = cobolGenerator['cobol_perform_until'];
cobolGenerator.forBlock['cobol_perform_varying'] = cobolGenerator['cobol_perform_varying'];
cobolGenerator.forBlock['cobol_goto'] = cobolGenerator['cobol_goto'];

// String Manipulation
cobolGenerator.forBlock['cobol_string_concatenate'] = cobolGenerator['cobol_string_concatenate'];
cobolGenerator.forBlock['cobol_unstring'] = cobolGenerator['cobol_unstring'];
cobolGenerator.forBlock['cobol_inspect'] = cobolGenerator['cobol_inspect'];

// Initialization and Utility
cobolGenerator.forBlock['cobol_initialize'] = cobolGenerator['cobol_initialize'];
cobolGenerator.forBlock['cobol_initialize_multiple'] = cobolGenerator['cobol_initialize_multiple'];
cobolGenerator.forBlock['cobol_set'] = cobolGenerator['cobol_set'];

// Program Control
cobolGenerator.forBlock['cobol_call'] = cobolGenerator['cobol_call'];
cobolGenerator.forBlock['cobol_call_using'] = cobolGenerator['cobol_call_using'];
cobolGenerator.forBlock['cobol_exit'] = cobolGenerator['cobol_exit'];
cobolGenerator.forBlock['cobol_exit_program'] = cobolGenerator['cobol_exit_program'];
cobolGenerator.forBlock['cobol_stop_run'] = cobolGenerator['cobol_stop_run'];
cobolGenerator.forBlock['cobol_return'] = cobolGenerator['cobol_return'];

// Error Handling
cobolGenerator.forBlock['cobol_on_size_error'] = cobolGenerator['cobol_on_size_error'];
cobolGenerator.forBlock['cobol_not_on_size_error'] = cobolGenerator['cobol_not_on_size_error'];
cobolGenerator.forBlock['cobol_invalid_key'] = cobolGenerator['cobol_invalid_key'];

// Sort and Merge
cobolGenerator.forBlock['cobol_sort'] = cobolGenerator['cobol_sort'];
cobolGenerator.forBlock['cobol_merge'] = cobolGenerator['cobol_merge'];

// Table Handling
cobolGenerator.forBlock['cobol_search'] = cobolGenerator['cobol_search'];
cobolGenerator.forBlock['cobol_search_all'] = cobolGenerator['cobol_search_all'];

// Report Writer
cobolGenerator.forBlock['cobol_report_section'] = cobolGenerator['cobol_report_section'];
cobolGenerator.forBlock['cobol_initiate'] = cobolGenerator['cobol_initiate'];
cobolGenerator.forBlock['cobol_generate'] = cobolGenerator['cobol_generate'];
cobolGenerator.forBlock['cobol_terminate'] = cobolGenerator['cobol_terminate'];

// Special COBOL Constructs
cobolGenerator.forBlock['cobol_copy'] = cobolGenerator['cobol_copy'];
cobolGenerator.forBlock['cobol_replace'] = cobolGenerator['cobol_replace'];
cobolGenerator.forBlock['cobol_exec_sql'] = cobolGenerator['cobol_exec_sql'];
cobolGenerator.forBlock['cobol_exec_cics'] = cobolGenerator['cobol_exec_cics'];

// Compiler Directives
cobolGenerator.forBlock['cobol_eject'] = cobolGenerator['cobol_eject'];
cobolGenerator.forBlock['cobol_skip'] = cobolGenerator['cobol_skip'];
cobolGenerator.forBlock['cobol_title'] = cobolGenerator['cobol_title'];