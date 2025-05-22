import * as Blockly from 'blockly';

Blockly.Blocks['cobol_identification_division'] = {
  init() {
    this.appendDummyInput().appendField('IDENTIFICATION DIVISION. PROGRAM-ID.')
        .appendField(new Blockly.FieldTextInput('BLOCKPROG'), 'PROG');
    this.setNextStatement(true);
    this.setColour(10);
  }
};

Blockly.Blocks['cobol_environment_division'] = {
  init() {
    this.appendDummyInput().appendField('ENVIRONMENT DIVISION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(20);
  }
};

Blockly.Blocks['cobol_data_division'] = {
  init() {
    this.appendDummyInput().appendField('DATA DIVISION.');
    this.appendDummyInput().appendField('WORKING-STORAGE SECTION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(30);
  }
};

Blockly.Blocks['cobol_variable_declaration'] = {
  init() {
    this.appendDummyInput()
      .appendField("DECLARE")
      .appendField(new Blockly.FieldTextInput("VAR-NAME"), "VAR")
      .appendField("PIC")
      .appendField(new Blockly.FieldDropdown([
        ["9(4)", "9(4)"],
        ["X(10)", "X(10)"],
        ["A(50)", "A(50)"]
      ]), "PIC");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(45);
  }
};


Blockly.Blocks['cobol_procedure_division'] = {
  init() {
    this.appendDummyInput().appendField('PROCEDURE DIVISION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(40);
  }
};

// Existing I/O + Logic Blocks...
Blockly.Blocks['cobol_display'] = {
  init() {
    this.appendDummyInput().appendField('DISPLAY')
      .appendField(new Blockly.FieldTextInput('Hello'), 'TEXT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(160);
  }
};

Blockly.Blocks['cobol_accept'] = {
  init: function () {
    this.appendDummyInput()
      .appendField("ACCEPT")
      .appendField(new Blockly.FieldTextInput("VAR-NAME"), "VAR");
    this.setPreviousStatement(true, null);
    this.setNextStatement(true, null);
    this.setColour(160);
    this.setTooltip("Accept user input and store it in a variable");
    this.setHelpUrl("");
  }
};


// Logic & Control
Blockly.Blocks['cobol_if'] = {
  init() {
    this.appendValueInput('COND').setCheck('String').appendField('IF');
    this.appendStatementInput('THEN').appendField('THEN');
    this.appendStatementInput('ELSE').appendField('ELSE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(210);
  }
};

// COBOL Paragraph
Blockly.Blocks['cobol_paragraph'] = {
  init() {
    this.appendDummyInput().appendField("Paragraph:")
      .appendField(new Blockly.FieldTextInput("PARA-NAME"), "PARA");
    this.appendStatementInput("BODY").setCheck(null);
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(110);
  }
};

// STRING concatenation
Blockly.Blocks['cobol_string_concatenate'] = {
  init() {
    this.appendDummyInput()
      .appendField("CONCATENATE")
      .appendField(new Blockly.FieldTextInput("VAR1"), "VAR1")
      .appendField("WITH")
      .appendField(new Blockly.FieldTextInput("VAR2"), "VAR2")
      .appendField("INTO")
      .appendField(new Blockly.FieldTextInput("RESULT"), "RESULT");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(65);
  }
};
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
    this.setNextStatement(true);
    this.setColour(10);
  }
};
Blockly.Blocks['cobol_configuration_section'] = {
  init() {
    this.appendDummyInput().appendField('CONFIGURATION SECTION.');
    this.appendDummyInput().appendField('SOURCE-COMPUTER:')
      .appendField(new Blockly.FieldTextInput('IBM-370'), 'SOURCE');
    this.appendDummyInput().appendField('OBJECT-COMPUTER:')
      .appendField(new Blockly.FieldTextInput('IBM-370'), 'OBJECT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(20);
  }
};
Blockly.Blocks['cobol_input_output_section'] = {
  init() {
    this.appendDummyInput().appendField('INPUT-OUTPUT SECTION.');
    this.appendDummyInput().appendField('FILE-CONTROL.');
    this.appendDummyInput().appendField('SELECT')
      .appendField(new Blockly.FieldTextInput('FILE-ID'), 'SELECT');
    this.appendDummyInput().appendField('ASSIGN TO')
      .appendField(new Blockly.FieldTextInput('filename.txt'), 'ASSIGN');
    this.appendDummyInput().appendField('ORGANIZATION IS')
      .appendField(new Blockly.FieldDropdown([["LINE SEQUENTIAL", "LINE SEQUENTIAL"], ["SEQUENTIAL", "SEQUENTIAL"]]), 'ORG');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(20);
  }
};
Blockly.Blocks['cobol_file_section'] = {
  init() {
    this.appendDummyInput().appendField('FILE SECTION.');
    this.appendDummyInput().appendField('FD')
      .appendField(new Blockly.FieldTextInput('MYFILE'), 'FD');
    this.appendDummyInput().appendField('01')
      .appendField(new Blockly.FieldTextInput('MYRECORD'), 'RECORD');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(30);
  }
};
Blockly.Blocks['cobol_open'] = {
  init() {
    this.appendDummyInput().appendField('OPEN')
      .appendField(new Blockly.FieldDropdown([["INPUT", "INPUT"], ["OUTPUT", "OUTPUT"], ["I-O", "I-O"]]), 'MODE')
      .appendField(new Blockly.FieldTextInput('MYFILE'), 'FILE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(140);
  }
};
Blockly.Blocks['cobol_return'] = {
  init() {
    this.appendDummyInput().appendField('RETURN');
    this.setPreviousStatement(true);
    this.setNextStatement(false);
    this.setColour(0);
  }
};


// INITIALIZE MULTIPLE VARS
Blockly.Blocks['cobol_initialize_multiple'] = {
  init() {
    this.appendDummyInput().appendField("INITIALIZE")
      .appendField(new Blockly.FieldTextInput("VAR1 VAR2"), "VARS");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
  }
};


Blockly.Blocks['cobol_when_case'] = {
  init() {
    this.appendDummyInput()
      .appendField("WHEN")
      .appendField(new Blockly.FieldTextInput("VALUE"), "VALUE");
    this.appendStatementInput("DO").appendField("DO");
    this.setPreviousStatement(true); // Allows chaining WHENs
    this.setNextStatement(true);
    this.setColour(230);
  }
};

Blockly.Blocks['cobol_evaluate'] = {
  init() {
    this.appendValueInput('VALUE')
      .setCheck('String') // Allow value blocks like variable names or constants
      .appendField('EVALUATE');
    this.appendStatementInput('WHEN')
      .appendField('WHEN CASES'); // Accept nested WHEN blocks
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(210);
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
        ["NOT =", "NOT ="]
      ]), "OP")
      .appendField(new Blockly.FieldTextInput("VAR2"), "RIGHT");
    this.setOutput(true, "String");
    this.setColour(200);
  }
};


Blockly.Blocks['cobol_stop_run'] = {
  init() {
    this.appendDummyInput().appendField('STOP RUN');
    this.setPreviousStatement(true);
    this.setColour(0);
  }
};

Blockly.Blocks['cobol_perform_until'] = {
  init() {
    this.appendValueInput('COND').setCheck('String').appendField('PERFORM UNTIL');
    this.appendStatementInput('DO').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(230);
  }
};

Blockly.Blocks['cobol_perform'] = {
  init() {
    this.appendDummyInput().appendField('PERFORM')
      .appendField(new Blockly.FieldTextInput('PARA-NAME'), 'PARA');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(230);
  }
};

// Arithmetic & Vars
Blockly.Blocks['cobol_move'] = {
  init() {
    this.appendDummyInput().appendField('MOVE')
      .appendField(new Blockly.FieldTextInput('value'), 'VAL')
      .appendField('TO')
      .appendField(new Blockly.FieldTextInput('VAR'), 'VAR');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
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
  }
};

Blockly.Blocks['cobol_initialize'] = {
  init() {
    this.appendDummyInput().appendField('INITIALIZE')
      .appendField(new Blockly.FieldTextInput('VAR'), 'VAR');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
  }
};

Blockly.Blocks['cobol_call'] = {
  init() {
    this.appendDummyInput().appendField('CALL')
      .appendField(new Blockly.FieldTextInput('"PROGRAM"'), 'PROG');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(100);
  }
};
