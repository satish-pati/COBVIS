import * as Blockly from 'blockly';

// =============================================
// IDENTIFICATION DIVISION BLOCKS
// =============================================

Blockly.Blocks['cobol_identification_division'] = {
  init() {
    this.appendDummyInput().appendField('IDENTIFICATION DIVISION.');
    this.appendDummyInput().appendField('PROGRAM-ID:')
      .appendField(new Blockly.FieldTextInput('BLOCKPROG'), 'PROG');
    this.appendDummyInput().appendField('AUTHOR:')
      .appendField(new Blockly.FieldTextInput('AUTHOR'), 'AUTHOR');
    this.appendDummyInput().appendField('INSTALLATION:')
      .appendField(new Blockly.FieldTextInput('INSTALLATION'), 'INSTALLATION');
    this.appendDummyInput().appendField('DATE-WRITTEN:')
      .appendField(new Blockly.FieldTextInput('2025-05-20'), 'WRITTEN');
    this.appendDummyInput().appendField('DATE-COMPILED:')
      .appendField(new Blockly.FieldTextInput('AUTO'), 'COMPILED');
    this.appendDummyInput().appendField('SECURITY:')
      .appendField(new Blockly.FieldTextInput('NONE'), 'SECURITY');
    this.setNextStatement(true);
    this.setColour(10);
    this.setTooltip('IDENTIFICATION DIVISION - Program identification');
  }
};

Blockly.Blocks['cobol_remarks'] = {
  init() {
    this.appendDummyInput().appendField('REMARKS:')
      .appendField(new Blockly.FieldTextInput('Program description'), 'REMARKS');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(10);
    this.setTooltip('Add remarks to program');
  }
};

// =============================================
// ENVIRONMENT DIVISION BLOCKS  
// =============================================

Blockly.Blocks['cobol_environment_division'] = {
  init() {
    this.appendDummyInput().appendField('ENVIRONMENT DIVISION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(20);
    this.setTooltip('ENVIRONMENT DIVISION - System environment');
  }
};

Blockly.Blocks['cobol_configuration_section'] = {
  init() {
    this.appendDummyInput().appendField('CONFIGURATION SECTION.');
    this.appendDummyInput().appendField('SOURCE-COMPUTER:')
      .appendField(new Blockly.FieldTextInput('IBM-370'), 'SOURCE');
    this.appendDummyInput().appendField('OBJECT-COMPUTER:')
      .appendField(new Blockly.FieldTextInput('IBM-370'), 'OBJECT');
    this.appendDummyInput().appendField('SPECIAL-NAMES:')
      .appendField(new Blockly.FieldTextInput('DECIMAL-POINT IS COMMA'), 'SPECIAL');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(20);
    this.setTooltip('Configuration section for system settings');
  }
};

Blockly.Blocks['cobol_input_output_section'] = {
  init() {
    this.appendDummyInput().appendField('INPUT-OUTPUT SECTION.');
    this.appendDummyInput().appendField('FILE-CONTROL.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(20);
    this.setTooltip('Input-Output section for file control');
  }
};

Blockly.Blocks['cobol_select_file'] = {
  init() {
    this.appendDummyInput().appendField('SELECT')
      .appendField(new Blockly.FieldTextInput('FILE-ID'), 'SELECT');
    this.appendDummyInput().appendField('ASSIGN TO')
      .appendField(new Blockly.FieldTextInput('filename.txt'), 'ASSIGN');
    this.appendDummyInput().appendField('ORGANIZATION IS')
      .appendField(new Blockly.FieldDropdown([
        ["SEQUENTIAL", "SEQUENTIAL"],
        ["LINE SEQUENTIAL", "LINE SEQUENTIAL"],
        ["INDEXED", "INDEXED"],
        ["RELATIVE", "RELATIVE"]
      ]), 'ORG');
    this.appendDummyInput().appendField('ACCESS MODE IS')
      .appendField(new Blockly.FieldDropdown([
        ["SEQUENTIAL", "SEQUENTIAL"],
        ["RANDOM", "RANDOM"],
        ["DYNAMIC", "DYNAMIC"]
      ]), 'ACCESS');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(20);
    this.setTooltip('Select file with organization and access mode');
  }
};

// =============================================
// DATA DIVISION BLOCKS
// =============================================

Blockly.Blocks['cobol_data_division'] = {
  init() {
    this.appendDummyInput().appendField('DATA DIVISION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(30);
    this.setTooltip('DATA DIVISION - Data definitions');
  }
};

Blockly.Blocks['cobol_file_section'] = {
  init() {
    this.appendDummyInput().appendField('FILE SECTION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(30);
    this.setTooltip('FILE SECTION for file record descriptions');
  }
};

Blockly.Blocks['cobol_fd_record'] = {
  init() {
    this.appendDummyInput().appendField('FD')
      .appendField(new Blockly.FieldTextInput('MYFILE'), 'FD');
    this.appendDummyInput().appendField('LABEL RECORDS ARE')
      .appendField(new Blockly.FieldDropdown([
        ["STANDARD", "STANDARD"],
        ["OMITTED", "OMITTED"]
      ]), 'LABEL');
    this.appendDummyInput().appendField('RECORD CONTAINS')
      .appendField(new Blockly.FieldTextInput('80'), 'SIZE')
      .appendField('CHARACTERS');
    this.appendDummyInput().appendField('01')
      .appendField(new Blockly.FieldTextInput('MYRECORD'), 'RECORD');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(30);
    this.setTooltip('File description with record layout');
  }
};

Blockly.Blocks['cobol_working_storage_section'] = {
  init() {
    this.appendDummyInput().appendField('WORKING-STORAGE SECTION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(30);
    this.setTooltip('WORKING-STORAGE SECTION for variables');
  }
};

Blockly.Blocks['cobol_variable_declaration'] = {
  init() {
    this.appendDummyInput()
      .appendField(new Blockly.FieldDropdown([
        ["01", "01"],
        ["05", "05"],
        ["10", "10"],
        ["15", "15"],
        ["20", "20"]
      ]), "LEVEL")
      .appendField(new Blockly.FieldTextInput("VAR-NAME"), "VAR")
      .appendField("PIC")
      .appendField(new Blockly.FieldDropdown([
        ["9(4)", "9(4)"],
        ["9(8)", "9(8)"],
        ["9(4)V99", "9(4)V99"],
        ["X(10)", "X(10)"],
        ["X(50)", "X(50)"],
        ["A(20)", "A(20)"],
        ["S9(4)", "S9(4)"],
        ["S9(8) COMP", "S9(8) COMP"]
      ]), "PIC");
    this.appendDummyInput()
      .appendField("VALUE")
      .appendField(new Blockly.FieldTextInput(""), "VALUE");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(45);
    this.setTooltip('Declare variable with level, picture and value');
  }
};

Blockly.Blocks['cobol_group_item'] = {
  init() {
    this.appendDummyInput()
      .appendField(new Blockly.FieldDropdown([
        ["01", "01"],
        ["05", "05"],
        ["10", "10"]
      ]), "LEVEL")
      .appendField(new Blockly.FieldTextInput("GROUP-NAME"), "NAME");
    this.appendStatementInput("ITEMS")
      .setCheck(null)
      .appendField("ITEMS:");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(45);
    this.setTooltip('Group data item with subordinate items');
  }
};

Blockly.Blocks['cobol_occurs_clause'] = {
  init() {
    this.appendDummyInput()
      .appendField(new Blockly.FieldDropdown([
        ["01", "01"],
        ["05", "05"],
        ["10", "10"]
      ]), "LEVEL")
      .appendField(new Blockly.FieldTextInput("ARRAY-NAME"), "NAME")
      .appendField("OCCURS")
      .appendField(new Blockly.FieldTextInput("10"), "TIMES")
      .appendField("TIMES");
    this.appendDummyInput()
      .appendField("PIC")
      .appendField(new Blockly.FieldDropdown([
        ["9(4)", "9(4)"],
        ["X(10)", "X(10)"],
        ["A(20)", "A(20)"]
      ]), "PIC");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(45);
    this.setTooltip('Array declaration with OCCURS clause');
  }
};

Blockly.Blocks['cobol_linkage_section'] = {
  init() {
    this.appendDummyInput().appendField('LINKAGE SECTION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(30);
    this.setTooltip('LINKAGE SECTION for parameters');
  }
};

// =============================================
// PROCEDURE DIVISION BLOCKS
// =============================================

Blockly.Blocks['cobol_procedure_division'] = {
  init() {
    this.appendDummyInput().appendField('PROCEDURE DIVISION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(40);
    this.setTooltip('PROCEDURE DIVISION - Program logic');
  }
};

Blockly.Blocks['cobol_procedure_using'] = {
  init() {
    this.appendDummyInput().appendField('PROCEDURE DIVISION USING')
      .appendField(new Blockly.FieldTextInput('PARM1 PARM2'), 'PARMS');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(40);
    this.setTooltip('PROCEDURE DIVISION with parameters');
  }
};

// =============================================
// PARAGRAPH AND SECTION BLOCKS
// =============================================

Blockly.Blocks['cobol_paragraph'] = {
  init() {
    this.appendDummyInput().appendField("Paragraph:")
      .appendField(new Blockly.FieldTextInput("PARA-NAME"), "PARA");
    this.appendStatementInput("BODY").setCheck(null);
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(110);
    this.setTooltip('COBOL paragraph with statements');
  }
};

Blockly.Blocks['cobol_section'] = {
  init() {
    this.appendDummyInput().appendField("Section:")
      .appendField(new Blockly.FieldTextInput("SECTION-NAME"), "SECTION");
    this.appendStatementInput("BODY").setCheck(null);
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(110);
    this.setTooltip('COBOL section with paragraphs');
  }
};

// =============================================
// INPUT/OUTPUT BLOCKS
// =============================================

Blockly.Blocks['cobol_display'] = {
  init() {
    this.appendDummyInput().appendField('DISPLAY')
      .appendField(new Blockly.FieldTextInput('Hello'), 'TEXT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(160);
    this.setTooltip('Display text or variable');
  }
};

Blockly.Blocks['cobol_display_variable'] = {
  init() {
    this.appendDummyInput().appendField('DISPLAY')
      .appendField(new Blockly.FieldTextInput('VAR-NAME'), 'VAR');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(160);
    this.setTooltip('Display variable value');
  }
};

Blockly.Blocks['cobol_accept'] = {
  init() {
    this.appendDummyInput()
      .appendField("ACCEPT")
      .appendField(new Blockly.FieldTextInput("VAR-NAME"), "VAR");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(160);
    this.setTooltip("Accept user input into variable");
  }
};

Blockly.Blocks['cobol_accept_date'] = {
  init() {
    this.appendDummyInput()
      .appendField("ACCEPT")
      .appendField(new Blockly.FieldTextInput("DATE-VAR"), "VAR")
      .appendField("FROM")
      .appendField(new Blockly.FieldDropdown([
        ["DATE", "DATE"],
        ["DAY", "DAY"],
        ["TIME", "TIME"],
        ["DAY-OF-WEEK", "DAY-OF-WEEK"]
      ]), "SOURCE");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(160);
    this.setTooltip("Accept system date/time");
  }
};

// =============================================
// FILE OPERATION BLOCKS
// =============================================

Blockly.Blocks['cobol_open'] = {
  init() {
    this.appendDummyInput().appendField('OPEN')
      .appendField(new Blockly.FieldDropdown([
        ["INPUT", "INPUT"],
        ["OUTPUT", "OUTPUT"],
        ["I-O", "I-O"],
        ["EXTEND", "EXTEND"]
      ]), 'MODE')
      .appendField(new Blockly.FieldTextInput('MYFILE'), 'FILE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(140);
    this.setTooltip('Open file for processing');
  }
};

Blockly.Blocks['cobol_close'] = {
  init() {
    this.appendDummyInput().appendField('CLOSE')
      .appendField(new Blockly.FieldTextInput('MYFILE'), 'FILE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(140);
    this.setTooltip('Close file');
  }
};

Blockly.Blocks['cobol_read'] = {
  init() {
    this.appendDummyInput().appendField('READ')
      .appendField(new Blockly.FieldTextInput('MYFILE'), 'FILE');
    this.appendDummyInput().appendField('AT END')
      .appendField(new Blockly.FieldTextInput('END-ACTION'), 'END_ACTION');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(140);
    this.setTooltip('Read record from file');
  }
};

Blockly.Blocks['cobol_write'] = {
  init() {
    this.appendDummyInput().appendField('WRITE')
      .appendField(new Blockly.FieldTextInput('RECORD-NAME'), 'RECORD');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(140);
    this.setTooltip('Write record to file');
  }
};

Blockly.Blocks['cobol_rewrite'] = {
  init() {
    this.appendDummyInput().appendField('REWRITE')
      .appendField(new Blockly.FieldTextInput('RECORD-NAME'), 'RECORD');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(140);
    this.setTooltip('Rewrite existing record');
  }
};

Blockly.Blocks['cobol_delete'] = {
  init() {
    this.appendDummyInput().appendField('DELETE')
      .appendField(new Blockly.FieldTextInput('FILE-NAME'), 'FILE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(140);
    this.setTooltip('Delete current record');
  }
};

// =============================================
// ARITHMETIC BLOCKS
// =============================================

Blockly.Blocks['cobol_move'] = {
  init() {
    this.appendDummyInput().appendField('MOVE')
      .appendField(new Blockly.FieldTextInput('value'), 'VAL')
      .appendField('TO')
      .appendField(new Blockly.FieldTextInput('VAR'), 'VAR');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Move value to variable');
  }
};

Blockly.Blocks['cobol_add'] = {
  init() {
    this.appendDummyInput().appendField('ADD')
      .appendField(new Blockly.FieldTextInput('1'), 'NUM')
      .appendField('TO')
      .appendField(new Blockly.FieldTextInput('VAR'), 'VAR');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Add number to variable');
  }
};

Blockly.Blocks['cobol_add_giving'] = {
  init() {
    this.appendDummyInput().appendField('ADD')
      .appendField(new Blockly.FieldTextInput('A'), 'A')
      .appendField('TO')
      .appendField(new Blockly.FieldTextInput('B'), 'B')
      .appendField('GIVING')
      .appendField(new Blockly.FieldTextInput('RESULT'), 'RESULT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Add two values giving result');
  }
};

Blockly.Blocks['cobol_subtract'] = {
  init() {
    this.appendDummyInput().appendField('SUBTRACT')
      .appendField(new Blockly.FieldTextInput('1'), 'NUM')
      .appendField('FROM')
      .appendField(new Blockly.FieldTextInput('VAR'), 'VAR');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Subtract number from variable');
  }
};

Blockly.Blocks['cobol_subtract_giving'] = {
  init() {
    this.appendDummyInput().appendField('SUBTRACT')
      .appendField(new Blockly.FieldTextInput('A'), 'A')
      .appendField('FROM')
      .appendField(new Blockly.FieldTextInput('B'), 'B')
      .appendField('GIVING')
      .appendField(new Blockly.FieldTextInput('RESULT'), 'RESULT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Subtract values giving result');
  }
};

Blockly.Blocks['cobol_multiply'] = {
  init() {
    this.appendDummyInput().appendField('MULTIPLY')
      .appendField(new Blockly.FieldTextInput('A'), 'A')
      .appendField('BY')
      .appendField(new Blockly.FieldTextInput('B'), 'B');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Multiply two values');
  }
};

Blockly.Blocks['cobol_multiply_giving'] = {
  init() {
    this.appendDummyInput().appendField('MULTIPLY')
      .appendField(new Blockly.FieldTextInput('A'), 'A')
      .appendField('BY')
      .appendField(new Blockly.FieldTextInput('B'), 'B')
      .appendField('GIVING')
      .appendField(new Blockly.FieldTextInput('RESULT'), 'RESULT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Multiply values giving result');
  }
};

Blockly.Blocks['cobol_divide'] = {
  init() {
    this.appendDummyInput().appendField('DIVIDE')
      .appendField(new Blockly.FieldTextInput('A'), 'A')
      .appendField('BY')
      .appendField(new Blockly.FieldTextInput('B'), 'B');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Divide two values');
  }
};

Blockly.Blocks['cobol_divide_giving'] = {
  init() {
    this.appendDummyInput().appendField('DIVIDE')
      .appendField(new Blockly.FieldTextInput('A'), 'A')
      .appendField('BY')
      .appendField(new Blockly.FieldTextInput('B'), 'B')
      .appendField('GIVING')
      .appendField(new Blockly.FieldTextInput('RESULT'), 'RESULT')
      .appendField('REMAINDER')
      .appendField(new Blockly.FieldTextInput('REM'), 'REMAINDER');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Divide values giving quotient and remainder');
  }
};

Blockly.Blocks['cobol_compute'] = {
  init() {
    this.appendDummyInput().appendField('COMPUTE')
      .appendField(new Blockly.FieldTextInput('RESULT'), 'RESULT')
      .appendField('=')
      .appendField(new Blockly.FieldTextInput('A + B * C'), 'EXPRESSION');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Compute arithmetic expression');
  }
};

// =============================================
// CONTROL FLOW BLOCKS
// =============================================

Blockly.Blocks['cobol_if'] = {
  init() {
    this.appendValueInput('COND').setCheck('String').appendField('IF');
    this.appendStatementInput('THEN').appendField('THEN');
    this.appendStatementInput('ELSE').appendField('ELSE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(210);
    this.setTooltip('IF-THEN-ELSE conditional statement');
  }
};

Blockly.Blocks['cobol_condition'] = {
  init() {
    this.appendDummyInput()
      .appendField(new Blockly.FieldTextInput("VAR1"), "LEFT")
      .appendField(new Blockly.FieldDropdown([
        ["=", "="],
        ["<", "<"],
        [">", ">"],
        ["<=", "<="],
        [">=", ">="],
        ["NOT =", "NOT ="],
        ["IS EQUAL TO", "IS EQUAL TO"],
        ["IS GREATER THAN", "IS GREATER THAN"],
        ["IS LESS THAN", "IS LESS THAN"]
      ]), "OP")
      .appendField(new Blockly.FieldTextInput("VAR2"), "RIGHT");
    this.setOutput(true, "String");
    this.setColour(200);
    this.setTooltip('Condition for IF statements');
  }
};

Blockly.Blocks['cobol_evaluate'] = {
  init() {
    this.appendValueInput('VALUE')
      .setCheck('String')
      .appendField('EVALUATE');
    this.appendStatementInput('WHEN')
      .appendField('WHEN CASES');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(210);
    this.setTooltip('EVALUATE statement for multiple conditions');
  }
};

Blockly.Blocks['cobol_when_case'] = {
  init() {
    this.appendDummyInput()
      .appendField("WHEN")
      .appendField(new Blockly.FieldTextInput("VALUE"), "VALUE");
    this.appendStatementInput("DO").appendField("DO");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(230);
    this.setTooltip('WHEN case for EVALUATE statement');
  }
};

Blockly.Blocks['cobol_when_other'] = {
  init() {
    this.appendDummyInput().appendField("WHEN OTHER");
    this.appendStatementInput("DO").appendField("DO");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(230);
    this.setTooltip('WHEN OTHER case for EVALUATE statement');
  }
};

// =============================================
// LOOP BLOCKS
// =============================================

Blockly.Blocks['cobol_perform'] = {
  init() {
    this.appendDummyInput().appendField('PERFORM')
      .appendField(new Blockly.FieldTextInput('PARA-NAME'), 'PARA');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(230);
    this.setTooltip('Perform paragraph');
  }
};

Blockly.Blocks['cobol_perform_times'] = {
  init() {
    this.appendDummyInput().appendField('PERFORM')
      .appendField(new Blockly.FieldTextInput('PARA-NAME'), 'PARA')
      .appendField(new Blockly.FieldTextInput('10'), 'TIMES')
      .appendField('TIMES');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(230);
    this.setTooltip('Perform paragraph specified number of times');
  }
};

Blockly.Blocks['cobol_perform_until'] = {
  init() {
    this.appendValueInput('COND').setCheck('String').appendField('PERFORM UNTIL');
    this.appendStatementInput('DO').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(230);
    this.setTooltip('Perform until condition is true');
  }
};

Blockly.Blocks['cobol_perform_varying'] = {
  init() {
    this.appendDummyInput().appendField('PERFORM VARYING')
      .appendField(new Blockly.FieldTextInput('I'), 'VAR')
      .appendField('FROM')
      .appendField(new Blockly.FieldTextInput('1'), 'FROM')
      .appendField('BY')
      .appendField(new Blockly.FieldTextInput('1'), 'BY')
      .appendField('UNTIL')
      .appendField(new Blockly.FieldTextInput('I > 10'), 'UNTIL');
    this.appendStatementInput('DO').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(230);
    this.setTooltip('Perform with varying counter');
  }
};

Blockly.Blocks['cobol_goto'] = {
  init() {
    this.appendDummyInput().appendField('GO TO')
      .appendField(new Blockly.FieldTextInput('PARA-NAME'), 'PARA');
    this.setPreviousStatement(true);
    this.setColour(230);
    this.setTooltip('Go to paragraph');
  }
};

// =============================================
// STRING MANIPULATION BLOCKS
// =============================================

Blockly.Blocks['cobol_string_concatenate'] = {
  init() {
    this.appendDummyInput()
      .appendField("STRING")
      .appendField(new Blockly.FieldTextInput("VAR1"), "VAR1")
      .appendField("DELIMITED BY")
      .appendField(new Blockly.FieldTextInput("SPACE"), "DELIM1");
    this.appendDummyInput()
      .appendField(new Blockly.FieldTextInput("VAR2"), "VAR2")
      .appendField("DELIMITED BY")
      .appendField(new Blockly.FieldTextInput("SIZE"), "DELIM2");
    this.appendDummyInput()
      .appendField("INTO")
      .appendField(new Blockly.FieldTextInput("RESULT"), "RESULT");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(65);
    this.setTooltip('Concatenate strings with delimiters');
  }
};

Blockly.Blocks['cobol_unstring'] = {
  init() {
    this.appendDummyInput()
      .appendField("UNSTRING")
      .appendField(new Blockly.FieldTextInput("SOURCE"), "SOURCE")
      .appendField("DELIMITED BY")
      .appendField(new Blockly.FieldTextInput("SPACE"), "DELIM");
    this.appendDummyInput()
      .appendField("INTO")
      .appendField(new Blockly.FieldTextInput("VAR1 VAR2"), "VARS");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(65);
    this.setTooltip('Split string into multiple variables');
  }
};

Blockly.Blocks['cobol_inspect'] = {
  init() {
    this.appendDummyInput()
      .appendField("INSPECT")
      .appendField(new Blockly.FieldTextInput("VAR"), "VAR")
      .appendField(new Blockly.FieldDropdown([
        ["TALLYING", "TALLYING"],
        ["REPLACING", "REPLACING"],
        ["CONVERTING", "CONVERTING"]
      ]), "ACTION");
    this.appendDummyInput()
      .appendField("FOR")
      .appendField(new Blockly.FieldTextInput("ALL"), "FOR")
      .appendField(new Blockly.FieldTextInput("'A'"), "CHAR");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(65);
    this.setTooltip('Inspect string for character operations');
  }
};

// =============================================
// INITIALIZATION AND UTILITY BLOCKS
// =============================================

Blockly.Blocks['cobol_initialize'] = {
  init() {
    this.appendDummyInput().appendField('INITIALIZE')
      .appendField(new Blockly.FieldTextInput('VAR'), 'VAR');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Initialize variable to default values');
  }
};

Blockly.Blocks['cobol_initialize_multiple'] = {
  init() {
    this.appendDummyInput().appendField("INITIALIZE")
      .appendField(new Blockly.FieldTextInput("VAR1 VAR2"), "VARS");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Initialize multiple variables');
  }
};

Blockly.Blocks['cobol_set'] = {
  init() {
    this.appendDummyInput().appendField('SET')
      .appendField(new Blockly.FieldTextInput('INDEX-VAR'), 'INDEX')
      .appendField('TO')
      .appendField(new Blockly.FieldTextInput('1'), 'VALUE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip('Set index or address variable');
  }
};

// =============================================
// PROGRAM CONTROL BLOCKS
// =============================================

Blockly.Blocks['cobol_call'] = {
  init() {
    this.appendDummyInput().appendField('CALL')
      .appendField(new Blockly.FieldTextInput('"PROGRAM"'), 'PROG');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(100);
    this.setTooltip('Call external program');
  }
};

Blockly.Blocks['cobol_call_using'] = {
  init() {
    this.appendDummyInput().appendField('CALL')
      .appendField(new Blockly.FieldTextInput('"PROGRAM"'), 'PROG')
      .appendField('USING')
      .appendField(new Blockly.FieldTextInput('PARM1 PARM2'), 'PARMS');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(100);
    this.setTooltip('Call external program with parameters');
  }
};

Blockly.Blocks['cobol_exit'] = {
  init() {
    this.appendDummyInput().appendField('EXIT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(0);
    this.setTooltip('Exit paragraph or section');
  }
};

Blockly.Blocks['cobol_exit_program'] = {
  init() {
    this.appendDummyInput().appendField('EXIT PROGRAM');
    this.setPreviousStatement(true);
    this.setColour(0);
    this.setTooltip('Exit current program');
  }
};

Blockly.Blocks['cobol_stop_run'] = {
  init() {
    this.appendDummyInput().appendField('STOP RUN');
    this.setPreviousStatement(true);
    this.setColour(0);
    this.setTooltip('Stop program execution');
  }
};

Blockly.Blocks['cobol_return'] = {
  init() {
    this.appendDummyInput().appendField('RETURN');
    this.setPreviousStatement(true);
    this.setNextStatement(false);
    this.setColour(0);
    this.setTooltip('Return from called program');
  }
};

// =============================================
// ERROR HANDLING BLOCKS
// =============================================

Blockly.Blocks['cobol_on_size_error'] = {
  init() {
    this.appendDummyInput().appendField('ON SIZE ERROR');
    this.appendStatementInput('ERROR_HANDLER').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(300);
    this.setTooltip('Handle arithmetic overflow errors');
  }
};

Blockly.Blocks['cobol_not_on_size_error'] = {
  init() {
    this.appendDummyInput().appendField('NOT ON SIZE ERROR');
    this.appendStatementInput('SUCCESS_HANDLER').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(300);
    this.setTooltip('Handle successful arithmetic operations');
  }
};

Blockly.Blocks['cobol_invalid_key'] = {
  init() {
    this.appendDummyInput().appendField('INVALID KEY');
    this.appendStatementInput('ERROR_HANDLER').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(300);
    this.setTooltip('Handle invalid key errors in file operations');
  }
};

// =============================================
// SORT AND MERGE BLOCKS
// =============================================

Blockly.Blocks['cobol_sort'] = {
  init() {
    this.appendDummyInput().appendField('SORT')
      .appendField(new Blockly.FieldTextInput('SORT-FILE'), 'SORT_FILE');
    this.appendDummyInput().appendField('ON')
      .appendField(new Blockly.FieldDropdown([
        ["ASCENDING KEY", "ASCENDING KEY"],
        ["DESCENDING KEY", "DESCENDING KEY"]
      ]), 'ORDER')
      .appendField(new Blockly.FieldTextInput('KEY-FIELD'), 'KEY');
    this.appendDummyInput().appendField('USING')
      .appendField(new Blockly.FieldTextInput('INPUT-FILE'), 'INPUT');
    this.appendDummyInput().appendField('GIVING')
      .appendField(new Blockly.FieldTextInput('OUTPUT-FILE'), 'OUTPUT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(180);
    this.setTooltip('Sort file records');
  }
};

Blockly.Blocks['cobol_merge'] = {
  init() {
    this.appendDummyInput().appendField('MERGE')
      .appendField(new Blockly.FieldTextInput('MERGE-FILE'), 'MERGE_FILE');
    this.appendDummyInput().appendField('ON')
      .appendField(new Blockly.FieldDropdown([
        ["ASCENDING KEY", "ASCENDING KEY"],
        ["DESCENDING KEY", "DESCENDING KEY"]
      ]), 'ORDER')
      .appendField(new Blockly.FieldTextInput('KEY-FIELD'), 'KEY');
    this.appendDummyInput().appendField('USING')
      .appendField(new Blockly.FieldTextInput('FILE1 FILE2'), 'INPUT');
    this.appendDummyInput().appendField('GIVING')
      .appendField(new Blockly.FieldTextInput('OUTPUT-FILE'), 'OUTPUT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(180);
    this.setTooltip('Merge multiple files');
  }
};

// =============================================
// TABLE HANDLING BLOCKS
// =============================================

Blockly.Blocks['cobol_search'] = {
  init() {
    this.appendDummyInput().appendField('SEARCH')
      .appendField(new Blockly.FieldTextInput('TABLE-NAME'), 'TABLE');
    this.appendDummyInput().appendField('AT END')
      .appendField(new Blockly.FieldTextInput('NOT-FOUND-ACTION'), 'AT_END');
    this.appendDummyInput().appendField('WHEN')
      .appendField(new Blockly.FieldTextInput('CONDITION'), 'WHEN');
    this.appendStatementInput('FOUND_ACTION').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(270);
    this.setTooltip('Sequential search of table');
  }
};

Blockly.Blocks['cobol_search_all'] = {
  init() {
    this.appendDummyInput().appendField('SEARCH ALL')
      .appendField(new Blockly.FieldTextInput('TABLE-NAME'), 'TABLE');
    this.appendDummyInput().appendField('AT END')
      .appendField(new Blockly.FieldTextInput('NOT-FOUND-ACTION'), 'AT_END');
    this.appendDummyInput().appendField('WHEN')
      .appendField(new Blockly.FieldTextInput('KEY = VALUE'), 'WHEN');
    this.appendStatementInput('FOUND_ACTION').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(270);
    this.setTooltip('Binary search of sorted table');
  }
};

// =============================================
// REPORT WRITER BLOCKS
// =============================================

Blockly.Blocks['cobol_report_section'] = {
  init() {
    this.appendDummyInput().appendField('REPORT SECTION.');
    this.appendDummyInput().appendField('RD')
      .appendField(new Blockly.FieldTextInput('REPORT-NAME'), 'REPORT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(320);
    this.setTooltip('Report description');
  }
};

Blockly.Blocks['cobol_initiate'] = {
  init() {
    this.appendDummyInput().appendField('INITIATE')
      .appendField(new Blockly.FieldTextInput('REPORT-NAME'), 'REPORT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(320);
    this.setTooltip('Initiate report processing');
  }
};

Blockly.Blocks['cobol_generate'] = {
  init() {
    this.appendDummyInput().appendField('GENERATE')
      .appendField(new Blockly.FieldTextInput('DETAIL-LINE'), 'LINE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(320);
    this.setTooltip('Generate report line');
  }
};

Blockly.Blocks['cobol_terminate'] = {
  init() {
    this.appendDummyInput().appendField('TERMINATE')
      .appendField(new Blockly.FieldTextInput('REPORT-NAME'), 'REPORT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(320);
    this.setTooltip('Terminate report processing');
  }
};

// =============================================
// SPECIAL COBOL CONSTRUCTS
// =============================================

Blockly.Blocks['cobol_copy'] = {
  init() {
    this.appendDummyInput().appendField('COPY')
      .appendField(new Blockly.FieldTextInput('COPYBOOK'), 'COPYBOOK');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(350);
    this.setTooltip('Include copybook');
  }
};

Blockly.Blocks['cobol_replace'] = {
  init() {
    this.appendDummyInput().appendField('COPY')
      .appendField(new Blockly.FieldTextInput('COPYBOOK'), 'COPYBOOK')
      .appendField('REPLACING')
      .appendField(new Blockly.FieldTextInput('==OLD=='), 'OLD')
      .appendField('BY')
      .appendField(new Blockly.FieldTextInput('==NEW=='), 'NEW');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(350);
    this.setTooltip('Include copybook with text replacement');
  }
};

Blockly.Blocks['cobol_exec_sql'] = {
  init() {
    this.appendDummyInput().appendField('EXEC SQL');
    this.appendDummyInput().appendField(new Blockly.FieldTextInput('SQL STATEMENT'), 'SQL');
    this.appendDummyInput().appendField('END-EXEC');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(380);
    this.setTooltip('Embedded SQL statement');
  }
};

Blockly.Blocks['cobol_exec_cics'] = {
  init() {
    this.appendDummyInput().appendField('EXEC CICS');
    this.appendDummyInput().appendField(new Blockly.FieldTextInput('CICS COMMAND'), 'CICS');
    this.appendDummyInput().appendField('END-EXEC');
    this.setPreviousStatement(true);
    this.setNextStrategy(true);
    this.setColour(380);
    this.setTooltip('Embedded CICS command');
  }
};

// =============================================
// COMPILER DIRECTIVES
// =============================================

Blockly.Blocks['cobol_eject'] = {
  init() {
    this.appendDummyInput().appendField('EJECT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(400);
    this.setTooltip('Page eject in source listing');
  }
};

Blockly.Blocks['cobol_skip'] = {
  init() {
    this.appendDummyInput().appendField('SKIP')
      .appendField(new Blockly.FieldTextInput('1'), 'LINES');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(400);
    this.setTooltip('Skip lines in source listing');
  }
};

Blockly.Blocks['cobol_title'] = {
  init() {
    this.appendDummyInput().appendField('TITLE')
      .appendField(new Blockly.FieldTextInput('Program Title'), 'TITLE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(400);
    this.setTooltip('Set listing title');
  }
};