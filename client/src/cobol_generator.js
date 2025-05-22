import * as Blockly from 'blockly';

export const cobolGenerator = new Blockly.Generator('COBOL');

cobolGenerator.scrub_ = (block, code) => code;
// Define required order constants
cobolGenerator.ORDER_ATOMIC = 0;
cobolGenerator.ORDER_NONE = 99;

// Divisions
cobolGenerator['cobol_identification_division'] = block =>
  `IDENTIFICATION DIVISION.\nPROGRAM-ID. ${block.getFieldValue('PROG')}.\n`;

cobolGenerator['cobol_environment_division'] = () =>
  `ENVIRONMENT DIVISION.\n`;

cobolGenerator['cobol_data_division'] = () =>
  `DATA DIVISION.\nWORKING-STORAGE SECTION.\n01 USER-INPUT PIC A(50).\n01 RESULT PIC 9(4).\n`;

cobolGenerator['cobol_variable_declaration'] = block => {
  const varName = block.getFieldValue('VAR');
  const pic = block.getFieldValue('PIC');
  return `01 ${varName} PIC ${pic}.\n`;
};

cobolGenerator['cobol_procedure_division'] = () =>
  `PROCEDURE DIVISION.\n`;

// I/O
cobolGenerator['cobol_display'] = block =>
  `DISPLAY "${block.getFieldValue('TEXT')}".\n`;

cobolGenerator['cobol_accept'] = function (block) {
  const varName = block.getFieldValue('VAR');
  return `ACCEPT ${varName}.\n`;
};

// COBOL Paragraph
cobolGenerator['cobol_paragraph'] = function (block) {
  const paraName = block.getFieldValue('PARA');
  const body = cobolGenerator.statementToCode(block, 'BODY');
  return `${paraName}.\n${body}`;
};

// CONCATENATE
cobolGenerator['cobol_string_concatenate'] = function (block) {
  const var1 = block.getFieldValue('VAR1');
  const var2 = block.getFieldValue('VAR2');
  const result = block.getFieldValue('RESULT');
  return `STRING ${var1} DELIMITED BY SPACE ${var2} DELIMITED BY SPACE INTO ${result}.\n`;
};

// INITIALIZE multiple vars
cobolGenerator['cobol_initialize_multiple'] = function (block) {
  const vars = block.getFieldValue('VARS');
  return `INITIALIZE ${vars}.\n`;
};


// Logic
cobolGenerator['cobol_if'] = function(block) {
  const cond = cobolGenerator.valueToCode(block, 'COND', cobolGenerator.ORDER_NONE) || 'CONDITION';
  const thenBranch = cobolGenerator.statementToCode(block, 'THEN');
  const elseBranch = cobolGenerator.statementToCode(block, 'ELSE');
  return `IF ${cond} THEN\n${thenBranch}ELSE\n${elseBranch}END-IF.\n`;
};


cobolGenerator['cobol_condition'] = function(block) {
  const left = block.getFieldValue('LEFT');
  const op = block.getFieldValue('OP');
  const right = block.getFieldValue('RIGHT');
  const code = `${left} ${op} ${right}`;
  return [code, cobolGenerator.ORDER_ATOMIC];
};
cobolGenerator['cobol_evaluate'] = function(block) {
  const val = cobolGenerator.valueToCode(block, 'VALUE', cobolGenerator.ORDER_NONE) || 'UNKNOWN';
  const body = cobolGenerator.statementToCode(block, 'WHEN');
  return `EVALUATE ${val}\n${body}END-EVALUATE.\n`;
};

cobolGenerator['cobol_when_case'] = function(block) {
  const val = block.getFieldValue('VALUE');
  const body = cobolGenerator.statementToCode(block, 'DO');
  return `  WHEN ${val}\n${body}`;
};


cobolGenerator['cobol_stop_run'] = () =>
  `STOP RUN.\n`;

//Loops
cobolGenerator['cobol_perform_until'] = block => {
  const cond = cobolGenerator.valueToCode(block, 'COND', 0);
  const body = cobolGenerator.statementToCode(block, 'DO');
  return `PERFORM UNTIL ${cond}\n${body}END-PERFORM.\n`;
};

cobolGenerator['cobol_perform'] = block =>
  `PERFORM ${block.getFieldValue('PARA')}.\n`;


// Variables
cobolGenerator['cobol_move'] = block =>
  `MOVE ${block.getFieldValue('VAL')} TO ${block.getFieldValue('VAR')}.\n`;

cobolGenerator['cobol_add'] = block =>
  `ADD ${block.getFieldValue('NUM')} TO ${block.getFieldValue('VAR')}.\n`;

cobolGenerator['cobol_subtract'] = block =>
  `SUBTRACT ${block.getFieldValue('NUM')} FROM ${block.getFieldValue('VAR')}.\n`;

cobolGenerator['cobol_multiply'] = block =>
  `MULTIPLY ${block.getFieldValue('A')} BY ${block.getFieldValue('B')}.\n`;

cobolGenerator['cobol_divide'] = block =>
  `DIVIDE ${block.getFieldValue('A')} BY ${block.getFieldValue('B')}.\n`;

cobolGenerator['cobol_initialize'] = block =>
  `INITIALIZE ${block.getFieldValue('VAR')}.\n`;

cobolGenerator['cobol_call'] = block =>
  `CALL ${block.getFieldValue('PROG')}.\n`;
cobolGenerator.forBlock['cobol_open'] = function(block) {
  const mode = block.getFieldValue('MODE');
  const file = block.getFieldValue('FILE');
  return `OPEN ${mode} ${file}.\n`;
};

// Register all blocks explicitly
cobolGenerator.forBlock['cobol_display'] = cobolGenerator['cobol_display'];
cobolGenerator.forBlock['cobol_accept'] = cobolGenerator['cobol_accept'];
cobolGenerator.forBlock['cobol_if'] = cobolGenerator['cobol_if'];
cobolGenerator.forBlock['cobol_stop_run'] = cobolGenerator['cobol_stop_run'];
cobolGenerator.forBlock['cobol_perform_until'] = cobolGenerator['cobol_perform_until'];
cobolGenerator.forBlock['cobol_move'] = cobolGenerator['cobol_move'];
cobolGenerator.forBlock['cobol_add'] = cobolGenerator['cobol_add'];

//  Additional blocks:
cobolGenerator.forBlock['cobol_subtract'] = cobolGenerator['cobol_subtract'];
cobolGenerator.forBlock['cobol_multiply'] = cobolGenerator['cobol_multiply'];
cobolGenerator.forBlock['cobol_divide'] = cobolGenerator['cobol_divide'];
cobolGenerator.forBlock['cobol_goto'] = cobolGenerator['cobol_goto'];
cobolGenerator.forBlock['cobol_evaluate'] = cobolGenerator['cobol_evaluate'];
cobolGenerator.forBlock['cobol_perform_varying'] = cobolGenerator['cobol_perform_varying'];

// Structural sections:
cobolGenerator.forBlock['cobol_identification_division'] = cobolGenerator['cobol_identification_division'];
cobolGenerator.forBlock['cobol_environment_division'] = cobolGenerator['cobol_environment_division'];
cobolGenerator.forBlock['cobol_data_division'] = cobolGenerator['cobol_data_division'];
cobolGenerator.forBlock['cobol_working_storage_section'] = cobolGenerator['cobol_working_storage_section'];
cobolGenerator.forBlock['cobol_procedure_division'] = cobolGenerator['cobol_procedure_division'];
cobolGenerator.forBlock['cobol_variable_declaration'] = cobolGenerator['cobol_variable_declaration'];
cobolGenerator.forBlock['cobol_condition'] = cobolGenerator['cobol_condition'];
cobolGenerator.forBlock['cobol_when_case'] = cobolGenerator['cobol_when_case'];
cobolGenerator.forBlock['cobol_paragraph'] = cobolGenerator['cobol_paragraph'];
cobolGenerator.forBlock['cobol_string_concatenate'] = cobolGenerator['cobol_string_concatenate'];
cobolGenerator.forBlock['cobol_initialize_multiple'] = cobolGenerator['cobol_initialize_multiple'];
