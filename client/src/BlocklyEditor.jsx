import * as Blockly from 'blockly';
import 'blockly/blocks';
import './cobol_blocks';
import { useEffect, useRef } from 'react';
import { cobolGenerator } from './cobol_generator';

export default function BlocklyEditor({ onCodeChange }) {
  const blocklyDiv = useRef(null);
  const workspaceRef = useRef(null);

  useEffect(() => {
    const workspace = Blockly.inject(blocklyDiv.current, {
      toolbox: document.getElementById('toolbox'),
    });
    workspaceRef.current = workspace;

    workspace.addChangeListener(() => {
      const bodyCode = cobolGenerator.workspaceToCode(workspace);
      onCodeChange(bodyCode);
    });

    return () => {
      if (workspaceRef.current) {
        workspaceRef.current.dispose();
      }
    };
  }, [onCodeChange]);

  return (
    <>
      <xml id="toolbox" style={{ display: 'none' }}>
        <category name="Program Structure">
          <block type="cobol_identification_division" />
          <block type="cobol_environment_division" />
          <block type="cobol_data_division" />
          <block type="cobol_procedure_division" />
          <block type="cobol_variable_declaration" />
        </category>
        <category name="I/O">
          <block type="cobol_display" />
          <block type="cobol_accept" />
        </category>
        <category name="Control Flow">
          <block type="cobol_if" />
          <block type="cobol_evaluate" />
            <block type="cobol_when_case" />
          <block type="cobol_stop_run" />
        </category>
        <category name="Conditions">
  <block type="cobol_condition" />
</category>
        <category name="Loops">
          <block type="cobol_perform_until" />
          <block type="cobol_perform" />
        </category>
        <category name="Variables">
          <block type="cobol_move" />
          <block type="cobol_add" />
          <block type="cobol_subtract" />
          <block type="cobol_multiply" />
          <block type="cobol_divide" />
          <block type="cobol_initialize" />
        </category>
        <category name="Procedure & Paragraphs">
  <block type="cobol_paragraph" />
</category>
<category name="Strings">
  <block type="cobol_string_concatenate" />
</category>
<category name="Utilities">
  <block type="cobol_initialize_multiple" />
</category>

        <category name="Subprograms">
          <block type="cobol_call" />
        </category>
        <category name="Environment Division">
  <block type="cobol_environment_division" />
  <block type="cobol_configuration_section" />
  <block type="cobol_input_output_section" />
</category>

<category name="Data Division">
  <block type="cobol_data_division" />
  <block type="cobol_file_section" />
  <block type="cobol_variable_declaration" />
</category>

<category name="File Handling">
  <block type="cobol_open" />
  <block type="cobol_read" />
  <block type="cobol_write" />
  <block type="cobol_rewrite" />
  <block type="cobol_close" />
</category>

      </xml>
      <div ref={blocklyDiv} style={{ height: '600px', width: '100%' }} />
    </>
  );
}
