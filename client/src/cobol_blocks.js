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
    this.setColour(110);
    this.setTooltip(`

 "IDENTIFICATION DIVISION.": "Starts the COBOL program. This division is mandatory and provides metadata about the program.",
  "PROGRAM-ID.": "Defines the name of the program. Mandatory field. Must be a valid COBOL identifier.",
  "AUTHOR.": "Optional. Specifies the name of the person or team who wrote the program.",
  "INSTALLATION.": "Optional. Indicates the name of the organization or site where the program was developed or is used.",
  "DATE-WRITTEN.": "Optional. Specifies the date on which the program was originally written.",
  "DATE-COMPILED.": "Optional. Indicates the last compilation date. 'AUTO' may be used to auto-fill the date if supported.",
  "SECURITY.": "Optional. Describes the program’s security level. Common values: NONE, CONFIDENTIAL, INTERNAL USE ONLY."
}`);
  }
};

Blockly.Blocks['cobol_remarks'] = {
  init() {
    this.appendDummyInput().appendField('REMARKS:')
      .appendField(new Blockly.FieldTextInput('Program description'), 'REMARKS');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(110);
    this.setTooltip(`REMARKS.: Optional. Used for general comments, program description, or notes. Ignored by the compiler.
\nSyntax: REMARKS. text.`);
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
    this.setTooltip(`ENVIRONMENT DIVISION – Describes system environment.
Syntax:
  ENVIRONMENT DIVISION.
Used before configuration and I/O sections to declare external system settings.`);
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
    this.setTooltip(`CONFIGURATION SECTION (Optional)- Defines compiler and hardware details.\nSyntax: CONFIGURATION SECTION.\n  SOURCE-COMPUTER. name.\n  OBJECT-COMPUTER. name.`);
  }
};

Blockly.Blocks['cobol_input_output_section'] = {
  init() {
    this.appendDummyInput().appendField('INPUT-OUTPUT SECTION.');
    this.appendDummyInput().appendField('FILE-CONTROL.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(20);
    this.setTooltip(`INPUT-OUTPUT SECTION (Optional, but required if file I/O is used)
   - Declares file handling and control mechanisms.
   - Syntax:
     INPUT-OUTPUT SECTION.
     FILE-CONTROL.                     (Required if using files)
     SELECT <file-id>
       ASSIGN TO <filename>
       ORGANIZATION IS <type>.         (Optional)
       ACCESS MODE IS <mode>.          (Optional)

Purpose:
- Configures system environment and file handling.
- Needed when the COBOL program deals with external files or specific machine features.`);
  }
};

Blockly.Blocks['cobol_select_file'] = {
  init() {
    this.appendDummyInput().appendField('SELECT')
      .appendField(new Blockly.FieldTextInput('FILE-ID'), 'SELECT');
    this.appendDummyInput().appendField('ASSIGN TO')
      .appendField(new Blockly.FieldTextInput('"filename.txt"'), 'ASSIGN');
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
    this.setTooltip(`SELECT Clause – Defines how a file is identified and accessed by the COBOL program.

Fields:
- SELECT <file-id> (Mandatory): Logical name for the file used in the program.
- ASSIGN TO <filename> (Mandatory): Physical file name or device assignment.
- ORGANIZATION IS <type> (Optional): Specifies file structure. Common values:
    - SEQUENTIAL: Records are stored one after another.
    - LINE SEQUENTIAL: Text lines terminated by newline (typical for text files).
    - INDEXED: Records accessed by key values.
    - RELATIVE: Records accessed by position.
- ACCESS MODE IS <mode> (Optional): Defines how records are accessed.
    - SEQUENTIAL: Access records in order.
    - RANDOM: Access specific records directly.
    - DYNAMIC: Allows both sequential and random access.

Note:
- ORGANIZATION and ACCESS MODE are optional but commonly used for defining file behavior.
- Required when using FILE-CONTROL in the INPUT-OUTPUT SECTION.`);
  }
}; 

Blockly.Blocks['cobol_environment_division'] = {
  init() {
    this.appendDummyInput().appendField('ENVIRONMENT DIVISION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(20);
    this.setTooltip(`ENVIRONMENT DIVISION – Declares the system and file environment for the program.

(Mandatory if your program interacts with external files or devices.)

Includes two main sections:

1. CONFIGURATION SECTION (Optional)
   - Describes computer system and language-related configuration.
   - Syntax:
     CONFIGURATION SECTION.
     SOURCE-COMPUTER. <system>.        (Optional)
     OBJECT-COMPUTER. <system>.        (Optional)
     SPECIAL-NAMES. <rules>.           (Optional)

2. INPUT-OUTPUT SECTION (Optional, but required if file I/O is used)
   - Declares file handling and control mechanisms.
   - Syntax:
     INPUT-OUTPUT SECTION.
     FILE-CONTROL.                     (Required if using files)
     SELECT <file-id>
       ASSIGN TO <filename>
       ORGANIZATION IS <type>.         (Optional)
       ACCESS MODE IS <mode>.          (Optional)

Purpose:
- Configures system environment and file handling.
- Needed when the COBOL program deals with external files or specific machine features.`);
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
    this.setColour(50	);
    this.setTooltip(`DATA DIVISION (Optional) but commonly used  for Declaring all data items used by the COBOL program.

Includes the following sections:

1. FILE SECTION (Optional)
   - Used to define input/output file structures.
   - Contains FD (File Description) entries.

2. FD RECORD (Optional, used within FILE SECTION)
   - Describes the structure of a file record.

3. WORKING-STORAGE SECTION (Optional but commonly used)
   - Declares variables and constants for internal program use.

4. VARIABLE DECLARATION (Optional)
   - Defines individual data items using levels (e.g., 01, 05, 77).

5. GROUP ITEM (Optional)
   - Groups related data items under a common name using levels.

6. OCCURS CLAUSE (Optional)
   - Defines arrays or tables of repeating items.

7. LINKAGE SECTION (Optional)
   - Used for parameters passed to the program (typically for called programs or subroutines).

Note:
- DATA DIVISION is mandatory in most programs unless no data is declared.
- Most sections inside are optional, but WORKING-STORAGE is very common.
- This block provides a high-level structure for COBOL data definitions.`);
  }
};

Blockly.Blocks['cobol_file_section'] = {
  init() {
    this.appendDummyInput().appendField('FILE SECTION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(50);
    this.setTooltip(`FILE SECTION(Optional) – Defines the layout of input/output files.
Syntax:
  FILE SECTION.
Used under DATA DIVISION to declare FD entries that describe file structure, record format, and block size.`);
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
    this.setColour(50);
    this.setTooltip(`FD <name>  (Optional, used within FILE SECTION)– Declares file descriptor and record layout.
Syntax:
  FD <name>
    LABEL RECORDS ARE <STANDARD|OMITTED>
    RECORD CONTAINS <n> CHARACTERS.
  01 <record-name>.
Used to define file format and record structure.`);
  }
};

Blockly.Blocks['cobol_working_storage_section'] = {
  init() {
    this.appendDummyInput().appendField('WORKING-STORAGE SECTION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(50);
    this.setTooltip(`WORKING-STORAGE SECTION (Optional but commonly used) – Declares global variables.
Used under DATA DIVISION to define variables that persist throughout program execution.
Syntax:
  WORKING-STORAGE SECTION.
  01 <var-name> PIC <type> VALUE <init>.`);
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
    this.setColour(50);
    this.setTooltip(`VARIABLE DECLARATION (Optional)- Declare a COBOL variable.
Specify level number, variable name, data type (PIC clause), and optional initial VALUE.
Example:
  01 AGE PIC 9(2) VALUE 25.`);
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
    this.setColour(50);
    this.setTooltip(`Group Item (Optional)– Combines multiple subordinate items under one name.
Used to group related data fields, allowing hierarchical data structures.
Syntax:
  <level> <group-name>.
    <sub-level> <field-name> PIC <type>.`);
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
    this.setColour(50);
    this.setTooltip(`OCCURS Clause (Optional) – Declares an array (repeating group) of data items.
Used to define a table or array in COBOL.
Syntax:
  <level> <array-name> OCCURS <n> TIMES
         PIC <type>.
Enables indexing and looping over repeated data.`);
  }
};

Blockly.Blocks['cobol_linkage_section'] = {
  init() {
    this.appendDummyInput().appendField('LINKAGE SECTION.');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(80);
    this.setTooltip(`LINKAGE SECTION(Optional) – Declares variables passed from other programs.
Used in subprograms to receive data through the CALL statement.
Variables must be defined in the PROCEDURE DIVISION USING clause.`);
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
    this.setColour(110);
    this.setTooltip(`PROCEDURE DIVISION (mandatory)– Contains the executable logic of the COBOL program.
Syntax:
  PROCEDURE DIVISION.
      <statements and paragraphs>
Purpose:
- Defines the sequence of operations to perform using declared data.
- Contains the core logic including conditionals, loops, and calls.`);
  }
};

Blockly.Blocks['cobol_procedure_using'] = {
  init() {
    this.appendDummyInput().appendField('PROCEDURE DIVISION USING')
      .appendField(new Blockly.FieldTextInput('PARM1 PARM2'), 'PARMS');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(110);
    this.setTooltip(`PROCEDURE DIVISION USING – Specifies parameters passed from another program or calling environment.
Syntax:
  PROCEDURE DIVISION USING <param1> <param2> ...
Purpose:
- Accepts data passed from calling program or linkage section.
- Used when working with subprograms or modular programs.`);
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
    this.setTooltip(`Paragraph – A named block of executable statements within the PROCEDURE DIVISION.
Syntax:
  <paragraph-name>.
      <statements>
Purpose:
- Organizes program logic into modular, reusable units.
- Can be invoked via PERFORM statements for structured flow control.`);
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
    this.setTooltip(`Section – A higher-level grouping of paragraphs within the PROCEDURE DIVISION.
Syntax:
  <section-name> SECTION.
      <paragraphs>
Purpose:
- Groups related paragraphs for readability and modular design.
- Helps structure complex programs into logical parts.`);
  }
};


// =============================================
// DISPLAY & ACCEPT BLOCKS
// =============================================
Blockly.Blocks['cobol_display'] = {
  init() {
    this.appendDummyInput().appendField('DISPLAY')
      .appendField(new Blockly.FieldTextInput('Hello'), 'TEXT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(330);
    this.setTooltip(
      `DISPLAY – Outputs literal text or expression to the console.
Syntax:
  DISPLAY <text-or-variable>.
Uses:
- Debugging and user messages.`
    );
  }
};
Blockly.Blocks['cobolif_display'] = {
  init() {
    this.appendDummyInput().appendField('DISPLAY')
      .appendField(new Blockly.FieldTextInput('Hello'), 'TEXT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(330);
    this.setTooltip(
      `DISPLAY – Outputs literal text or expression to the console.
Syntax:
  DISPLAY <text-or-variable>.
Uses:
- Debugging and user messages.`
    );
  }
};

Blockly.Blocks['cobol_display_variable'] = {
  init() {
    this.appendDummyInput().appendField('DISPLAY')
      .appendField(new Blockly.FieldTextInput('VAR-NAME'), 'VAR');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(330);
    this.setTooltip(
      `DISPLAY (Variable) – Shows the value of a variable.
Syntax:
  DISPLAY <var-name>.
Uses:
- Output data stored in working-storage or linkage variables.`
    );
  }
};

Blockly.Blocks['cobol_accept'] = {
  init() {
    this.appendDummyInput()
      .appendField("ACCEPT")
      .appendField(new Blockly.FieldTextInput("VAR-NAME"), "VAR");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(330);
    this.setTooltip(
      `ACCEPT – Reads user input into a variable.
Syntax:
  ACCEPT <var-name>.
Uses:
- Interactive data entry from terminal.`
    );
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
    this.setColour(330);
    this.setTooltip(
      `ACCEPT Date/Time – Retrieves system date, day, time, or weekday.
Syntax:
  ACCEPT <var> FROM <DATE|DAY|TIME|DAY-OF-WEEK>.
Uses:
- Populate variables with runtime system values.`
    );
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
    this.setTooltip(
      `OPEN – Prepares file for processing in specified mode.
Syntax:
  OPEN {INPUT|OUTPUT|I-O|EXTEND} <file-id>.
Uses:
- Initialize file access before READ/WRITE/REWRITE.`
    );
  }
};

Blockly.Blocks['cobol_close'] = {
  init() {
    this.appendDummyInput().appendField('CLOSE')
      .appendField(new Blockly.FieldTextInput('MYFILE'), 'FILE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(140);
    this.setTooltip(
      `CLOSE – Terminates processing of an open file.
Syntax:
  CLOSE <file-id>.
Uses:
- Release file resources after I/O operations.`
    );
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
    this.setTooltip(
      `READ – Reads next record from file into record area.
Syntax:
  READ <file-id> [AT END <action>].
Uses:
- Sequential file record retrieval; handle end-of-file.`
    );
  }
};

Blockly.Blocks['cobol_write'] = {
  init() {
    this.appendDummyInput().appendField('WRITE')
      .appendField(new Blockly.FieldTextInput('RECORD-NAME'), 'RECORD');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(140);
    this.setTooltip(
      `WRITE – Writes record area contents to output file.
Syntax:
  WRITE <record-name>.
Uses:
- Generate new file records in OUTPUT or EXTEND mode.`
    );
  }
};

Blockly.Blocks['cobol_rewrite'] = {
  init() {
    this.appendDummyInput().appendField('REWRITE')
      .appendField(new Blockly.FieldTextInput('RECORD-NAME'), 'RECORD');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(140);
    this.setTooltip(
      `REWRITE – Updates current record in I-O file.
Syntax:
  REWRITE <record-name>.
Uses:
- Modify existing record after READ.`
    );
  }
};

Blockly.Blocks['cobol_delete'] = {
  init() {
    this.appendDummyInput().appendField('DELETE')
      .appendField(new Blockly.FieldTextInput('FILE-NAME'), 'FILE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(140);
    this.setTooltip(
      `DELETE – Removes current record in I-O file.
Syntax:
  DELETE <file-id>.
Uses:
- Logical deletion of record in indexed files.`
    );
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
    this.setTooltip(
      `MOVE – Assigns source value to target variable.
Syntax:
  MOVE <source> TO <destination>.
Uses:
- Data transfer between fields.`
    );
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
    this.setTooltip(
      `ADD – Increments variable by numeric value.
Syntax:
  ADD <number> TO <var>.
Uses:
- Simple accumulation of values.`
    );
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
    this.setTooltip(
      `ADD GIVING – Adds two values and stores sum in result.
Syntax:
  ADD <A> TO <B> GIVING <RESULT>.
Uses:
- Non-destructive addition.`
    );
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
    this.setTooltip(
      `SUBTRACT – Decrements variable by numeric value.
Syntax:
  SUBTRACT <number> FROM <var>.
Uses:
- Simple subtraction operations.`
    );
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
    this.setTooltip(
      `SUBTRACT GIVING – Subtracts B from A and stores difference.
Syntax:
  SUBTRACT <A> FROM <B> GIVING <RESULT>.
Uses:
- Non-destructive subtraction.`
    );
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
    this.setTooltip(
      `MULTIPLY – Multiplies two numeric values.
Syntax:
  MULTIPLY <A> BY <B>.
Uses:
- Basic multiplication operations.`
    );
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
    this.setTooltip(
      `MULTIPLY GIVING – Multiplies A and B, stores product.
Syntax:
  MULTIPLY <A> BY <B> GIVING <RESULT>.
Uses:
- Non-destructive multiplication.`
    );
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
    this.setTooltip(
      `DIVIDE – Divides A by B, quotient stored in A.
Syntax:
  DIVIDE <A> BY <B>.
Uses:
- Basic division operations.`
    );
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
    this.setTooltip(
      `DIVIDE GIVING – Divides A by B, stores quotient and remainder.
Syntax:
  DIVIDE <A> BY <B> GIVING <RESULT> REMAINDER <R>.
Uses:
- Quotient and remainder extraction.`
    );
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
    this.setTooltip(
      `COMPUTE – Evaluates arithmetic expression and stores in result.
Syntax:
  COMPUTE <result> = <expression>.
Uses:
- Complex calculations with operators.`
    );
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
    this.setTooltip(
      `IF – Conditional branching.
Syntax:
  IF <condition> THEN <statements> [ELSE <statements>].
Uses:
- Control program flow based on data.`
    );
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
    this.setTooltip(
      `Condition – Comparison operator expression.
Syntax:
  <left> {=|<>|>|<|<=|>=} <right>.
Uses:
- Build conditions for IF, PERFORM UNTIL, EVALUATE.`
    );
  }
};
/*
Blockly.Blocks['cobol_evaluate'] = {
  init() {
    this.appendValueInput('VALUE')
      .setCheck('String')
      .appendField('EVALUATE');
    this.appendStatementInput('WHEN')
      .appendField('WHEN');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(210);
    this.setTooltip(
      `EVALUATE – Multi-case conditional (switch).
Syntax:
  EVALUATE <expr>
    WHEN <val> => <stmts>
    WHEN OTHER => <stmts>
  END-EVALUATE.
Uses:
- Simplify multiple condition branching.`
    );
  }
};*/
Blockly.Blocks['cobol_evaluate'] = {
  init() {
    // 1) Create a single dummy input with a variable‐field for EVALUATE <var-name>
    this.appendDummyInput()
        .appendField('EVALUATE')
              .appendField(new Blockly.FieldTextInput('VAR-NAME'), 'VAR');

    // 2) Keep the WHEN … statements as before
    this.appendStatementInput('WHEN')
        .appendField('WHEN');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(210);
    this.setTooltip(
      `EVALUATE – Multi‐case conditional (switch).\n` +
      `Syntax:\n` +
      `  EVALUATE <identifier>\n` +
      `    WHEN <value> => <statements>\n` +
      `    WHEN OTHER => <statements>\n` +
      `  END‐EVALUATE.\n` +
      `Uses:\n` +
      `- Simplify multiple condition branching, using a COBOL variable (or literal) to compare.`
    );
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
    this.setColour(210);
    this.setTooltip(
      `WHEN – Case clause for EVALUATE.
Syntax:
  WHEN <value> => <statements>.
Uses:
- Define action for specific cases.`
    );
  }
};

Blockly.Blocks['cobol_when_other'] = {
  init() {
    this.appendDummyInput().appendField("WHEN OTHER");
    this.appendStatementInput("DO").appendField("DO");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(210);
    this.setTooltip(
      `WHEN OTHER – Default case for EVALUATE.
Syntax:
  WHEN OTHER => <statements>.
Uses:
- Handle unmatched cases in EVALUATE.`
    );
  }
};

// =============================================
// LOOP & PERFORM BLOCKS
// =============================================
Blockly.Blocks['cobol_end_perform'] = {
  init() {
    this.appendDummyInput().appendField('END-PERFORM');
    this.setPreviousStatement(true);
    this.setColour(230);
    this.setTooltip(
      `END-PERFORM – Terminates a PERFORM block.\n` +
      `Use after conditional or looping PERFORM constructs.`
    );
  }
};

Blockly.Blocks['cobol_perform'] = {
  init() {
    this.appendDummyInput().appendField('PERFORM')
      .appendField(new Blockly.FieldTextInput('PARA-NAME'), 'PARA');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(230);
    this.setTooltip(
      `PERFORM – Executes a paragraph or section.
Syntax:
  PERFORM <para-or-section>.
Uses:
- Modularize execution flow.`
    );
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
    this.setTooltip(
      `PERFORM TIMES – Repeats paragraph n times.
Syntax:
  PERFORM <para> <n> TIMES.
Uses:
- Loop fixed number of iterations.`
    );
  }
};

Blockly.Blocks['cobol_perform_until'] = {
  init() {
    this.appendValueInput('COND').setCheck('String').appendField('PERFORM UNTIL');
    this.appendStatementInput('DO').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(230);
    this.setTooltip(
      `PERFORM UNTIL – Loop until condition true.
Syntax:
  PERFORM UNTIL <cond>
    <statements>
  END-PERFORM.
Uses:
- Conditional loops.`
    );
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
    this.setTooltip(
      `PERFORM VARYING – Counter-controlled loop.
Syntax:
  PERFORM VARYING <var> FROM <start> BY <inc> UNTIL <cond>.
Uses:
- Iterate with index variable.`
    );
  }
};

Blockly.Blocks['cobol_goto'] = {
  init() {
    this.appendDummyInput().appendField('GO TO')
      .appendField(new Blockly.FieldTextInput('PARA-NAME'), 'PARA');
    this.setPreviousStatement(true);
    this.setColour(230);
    this.setTooltip(
      `GO TO – Unconditional jump to paragraph.
Syntax:
  GO TO <para-name>.
Uses:
- Transfer control; use sparingly for clarity.`
    );
  }
};

// =============================================
// STRING MANIPULATION
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
    this.setColour(310);
    this.setTooltip(
      `STRING – Concatenate source fields into a result.
Syntax:
  STRING <var1> DELIMITED BY <delim1>
         <var2> DELIMITED BY <delim2> INTO <result>.
Uses:
- Build composite text.`
    );
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
    this.setColour(310);
    this.setTooltip(
      `UNSTRING – Split source into variables by delimiter.
Syntax:
  UNSTRING <source> DELIMITED BY <delim> INTO <var-list>.
Uses:
- Parse text into fields.`
    );
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
    this.setColour(310);
    this.setTooltip(
      `INSPECT – Analyze or modify characters in a string.
Syntax:
  INSPECT <var> {TALLYING|REPLACING|CONVERTING}
    FOR <ALL|LEADING|FIRST> <char>.
Uses:
- Character counts, replacements, conversions.`
    );
  }
};

// =============================================
// INITIALIZE & SET
// =============================================
Blockly.Blocks['cobol_initialize'] = {
  init() {
    this.appendDummyInput().appendField('INITIALIZE')
      .appendField(new Blockly.FieldTextInput('VAR'), 'VAR');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip(
      `INITIALIZE – Reset numeric items to zero and alphas to spaces.
Syntax:
  INITIALIZE <var>.
Uses:
- Clear working-storage fields.`
    );
  }
};

Blockly.Blocks['cobol_initialize_multiple'] = {
  init() {
    this.appendDummyInput().appendField("INITIALIZE")
      .appendField(new Blockly.FieldTextInput("VAR1 VAR2"), "VARS");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(60);
    this.setTooltip(
      `INITIALIZE – Reset multiple fields at once.
Syntax:
  INITIALIZE <var-list>.
Uses:
- Bulk clearing of data items.`
    );
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
    this.setTooltip(
      `SET – Assign starting value to index or pointer.
Syntax:
  SET <index-var> TO <value>.
Uses:
- Initialize OCCURS index or pointers.`
    );
  }
};

// =============================================
// CALL & EXIT
// =============================================
Blockly.Blocks['cobol_call'] = {
  init() {
    this.appendDummyInput().appendField('CALL')
      .appendField(new Blockly.FieldTextInput('"PROGRAM"'), 'PROG');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(0);
    this.setTooltip(
      `CALL – Invoke external subprogram.
Syntax:
  CALL "prog-name".
Uses:
- Modular program structures.`
    );
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
    this.setColour(0);
    this.setTooltip(
      `CALL USING – Invoke subprogram with parameters.
Syntax:
  CALL "prog" USING <parm-list>.
Uses:
- Pass data via linkage section.`
    );
  }
};

Blockly.Blocks['cobol_exit'] = {
  init() {
    this.appendDummyInput().appendField('EXIT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(0);
    this.setTooltip(
      `EXIT – Return from paragraph or section to caller.
Syntax:
  EXIT.
Uses:
- End internal program modules.`
    );
  }
};

Blockly.Blocks['cobol_exit_program'] = {
  init() {
    this.appendDummyInput().appendField('EXIT PROGRAM');
    this.setPreviousStatement(true);
    this.setColour(0);
    this.setTooltip(
      `EXIT PROGRAM – Terminate subprogram and return to main.
Syntax:
  EXIT PROGRAM.
Uses:
- End called program execution.`
    );
  }
};

Blockly.Blocks['cobol_stop_run'] = {
  init() {
    this.appendDummyInput().appendField('STOP RUN');
    this.setPreviousStatement(true);
    this.setColour(0);
    this.setTooltip(
      `STOP RUN – Ends program execution entirely.
Syntax:
  STOP RUN.
Uses:
- Final termination of main program.`
    );
  }
};

Blockly.Blocks['cobol_return'] = {
  init() {
    this.appendDummyInput().appendField('RETURN');
    this.setPreviousStatement(true);
    this.setNextStatement(false);
    this.setColour(0);
    this.setTooltip(
      `RETURN – Return to calling environment from program.
Syntax:
  RETURN.
Uses:
- End of called program, pass control back.`
    );
  }
};

// =============================================
// ERROR HANDLING
// =============================================
Blockly.Blocks['cobol_on_size_error'] = {
  init() {
    this.appendDummyInput().appendField('ON SIZE ERROR');
    this.appendStatementInput('ERROR_HANDLER').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(300);
    this.setTooltip(
      `ON SIZE ERROR – Handle numeric overflow or underflow.
Syntax:
  ON SIZE ERROR
    <statements>.
Uses:
- Error trapping in arithmetic operations.`
    );
  }
};

Blockly.Blocks['cobol_not_on_size_error'] = {
  init() {
    this.appendDummyInput().appendField('NOT ON SIZE ERROR');
    this.appendStatementInput('SUCCESS_HANDLER').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(300);
    this.setTooltip(
      `NOT ON SIZE ERROR – Execute when arithmetic succeeds.
Syntax:
  NOT ON SIZE ERROR
    <statements>.
Uses:
- Continue normal flow after arithmetic.`
    );
  }
};

Blockly.Blocks['cobol_invalid_key'] = {
  init() {
    this.appendDummyInput().appendField('INVALID KEY');
    this.appendStatementInput('ERROR_HANDLER').appendField('DO');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(300);
    this.setTooltip(
      `INVALID KEY – Handle database or indexed file errors.
Syntax:
  INVALID KEY
    <statements>.
Uses:
- Error handling for READ/WRITE on indexed files.`
    );
  }
};

// =============================================
// SORT & MERGE
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
    this.setTooltip(
      `SORT – Sort records of a file into new file.
Syntax:
  SORT <sort-file>
    ON {ASCENDING|DESCENDING} KEY <field>
    USING <in-file>
    GIVING <out-file>.
Uses:
- Batch ordering of data.`
    );
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
    this.setTooltip(
      `MERGE – Combine two or more sorted files into one.
Syntax:
  MERGE <merge-file>
    ON {ASCENDING|DESCENDING} KEY <field>
    USING <in-file-list>
    GIVING <out-file>.
Uses:
- Efficient merge of sorted datasets.`
    );
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
    this.setColour(120	);
    this.setTooltip(
      `SEARCH – Sequential search in table (OCCURS).
Syntax:
  SEARCH <table>
    AT END <action>
    WHEN <cond> DO <statements>.
Uses:
- Iterate table until condition or end reached.`
    );
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
    this.setColour(120	);
    this.setTooltip(
      `SEARCH ALL – Binary search on sorted table.
Syntax:
  SEARCH ALL <table>
    AT END <action>
    WHEN <cond> DO <statements>.
Uses:
- Fast lookup in sorted OCCURS tables.`
    );
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
    this.setTooltip(
      `REPORT SECTION – Define structure for report writer.
Syntax:
  REPORT SECTION.
    RD <report-name>.
Uses:
- Initialize report definitions.`
    );
  }
};

Blockly.Blocks['cobol_initiate'] = {
  init() {
    this.appendDummyInput().appendField('INITIATE')
      .appendField(new Blockly.FieldTextInput('REPORT-NAME'), 'REPORT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(320);
    this.setTooltip(
      `INITIATE – Start report processing at heading.
Syntax:
  INITIATE <report-name>.
Uses:
- Trigger report generation.`
    );
  }
};

Blockly.Blocks['cobol_generate'] = {
  init() {
    this.appendDummyInput().appendField('GENERATE')
      .appendField(new Blockly.FieldTextInput('DETAIL-LINE'), 'LINE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(320);
    this.setTooltip(
      `GENERATE – Emit a detail line in report.
Syntax:
  GENERATE <detail-line-name>.
Uses:
- Output report records.`
    );
  }
};

Blockly.Blocks['cobol_terminate'] = {
  init() {
    this.appendDummyInput().appendField('TERMINATE')
      .appendField(new Blockly.FieldTextInput('REPORT-NAME'), 'REPORT');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(320);
    this.setTooltip(
      `TERMINATE – End report processing with trailer.
Syntax:
  TERMINATE <report-name>.
Uses:
- Close out report.`
    );
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
    this.setColour(200);
    this.setTooltip(
      `COPY – Include external copybook into source.
Syntax:
  COPY <copybook>.
Uses:
- Reuse common declarations.`
    );
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
    this.setColour(200);
    this.setTooltip(
      `COPY REPLACING – Include copybook with text substitution.
Syntax:
  COPY <copybook> REPLACING ==old== BY ==new==.
Uses:
- Parameterize copybooks.`
    );
  }
};

Blockly.Blocks['cobol_exec_sql'] = {
  init() {
    this.appendDummyInput().appendField('EXEC SQL');
    this.appendDummyInput().appendField(new Blockly.FieldTextInput('SQL STATEMENT'), 'SQL');
    this.appendDummyInput().appendField('END-EXEC');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(200);
    this.setTooltip(
      `EXEC SQL – Embed SQL inside COBOL.
Syntax:
  EXEC SQL <sql> END-EXEC.
Uses:
- Database integration.`
    );
  }
};

Blockly.Blocks['cobol_exec_cics'] = {
  init() {
    this.appendDummyInput().appendField('EXEC CICS');
    this.appendDummyInput().appendField(new Blockly.FieldTextInput('CICS COMMAND'), 'CICS');
    this.appendDummyInput().appendField('END-EXEC');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(200);
    this.setTooltip(
      `EXEC CICS – Embed CICS commands in COBOL.
Syntax:
  EXEC CICS <command> END-EXEC.
Uses:
- Mainframe transaction processing.`
    );
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
    this.setColour(50	);
    this.setTooltip(
      `EJECT – Force page break in source listing.
Syntax:
  EJECT.
Uses:
- Organize printed listings.`
    );
  }
};

Blockly.Blocks['cobol_skip'] = {
  init() {
    this.appendDummyInput().appendField('SKIP')
      .appendField(new Blockly.FieldTextInput('1'), 'LINES');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(50	);
    this.setTooltip(
      `SKIP – Advance listing by given lines.
Syntax:
  SKIP <n>.
Uses:
- Format printed source.`
    );
  }
};

Blockly.Blocks['cobol_title'] = {
  init() {
    this.appendDummyInput().appendField('TITLE')
      .appendField(new Blockly.FieldTextInput('Program Title'), 'TITLE');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(50	);
    this.setTooltip(
      `TITLE – Set title for source listing.
Syntax:
  TITLE "<text>".
Uses:
- Document program name in listings.`
    );
  }
};
