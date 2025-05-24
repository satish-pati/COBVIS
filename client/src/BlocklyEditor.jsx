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
      collapse: true,
      comments: true,
      disable: true,
      maxBlocks: Infinity,
      trashcan: true,
      horizontalLayout: false,
      toolboxPosition: 'start',
      css: true,
      media: 'https://blockly-demo.appspot.com/static/media/',
      rtl: false,
      scrollbars: true,
      sounds: true,
      oneBasedIndex: true,
      grid: {
        spacing: 25,
        length: 3,
        colour: '#ccc',
        snap: true
      },
      zoom: {
        controls: true,
        wheel: true,
        startScale: 1.0,
        maxScale: 3,
        minScale: 0.3,
        scaleSpeed: 1.2
      }
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
        {/* IDENTIFICATION DIVISION */}
        <category name="📋 Identification Division" colour="10">
          <block type="cobol_identification_division" />
          <block type="cobol_remarks" />
        </category>

        {/* ENVIRONMENT DIVISION */}
        <category name="🌐 Environment Division" colour="20">
          <block type="cobol_environment_division" />
          <block type="cobol_configuration_section" />
          <block type="cobol_input_output_section" />
          <block type="cobol_select_file" />
        </category>

        {/* DATA DIVISION */}
        <category name="📊 Data Division" colour="30">
          <block type="cobol_data_division" />
          <block type="cobol_file_section" />
          <block type="cobol_fd_record" />
          <block type="cobol_working_storage_section" />
          <block type="cobol_variable_declaration" />
          <block type="cobol_group_item" />
          <block type="cobol_occurs_clause" />
          <block type="cobol_linkage_section" />
        </category>

        {/* PROCEDURE DIVISION */}
        <category name="⚙️ Procedure Division" colour="40">
          <block type="cobol_procedure_division" />
          <block type="cobol_procedure_using" />
          <block type="cobol_paragraph" />
          <block type="cobol_section" />
        </category>

        {/* INPUT/OUTPUT */}
        <category name="📺 Input/Output" colour="160">
          <block type="cobol_display" />
          <block type="cobol_display_variable" />
          <block type="cobol_accept" />
          <block type="cobol_accept_date" />
        </category>

        {/* FILE OPERATIONS */}
        <category name="📁 File Operations" colour="140">
          <block type="cobol_open" />
          <block type="cobol_close" />
          <block type="cobol_read" />
          <block type="cobol_write" />
          <block type="cobol_rewrite" />
          <block type="cobol_delete" />
        </category>

        {/* ARITHMETIC OPERATIONS */}
        <category name="🔢 Arithmetic" colour="60">
          <block type="cobol_move" />
          <block type="cobol_add" />
          <block type="cobol_add_giving" />
          <block type="cobol_subtract" />
          <block type="cobol_subtract_giving" />
          <block type="cobol_multiply" />
          <block type="cobol_multiply_giving" />
          <block type="cobol_divide" />
          <block type="cobol_divide_giving" />
          <block type="cobol_compute" />
        </category>

        {/* CONTROL FLOW */}
        <category name="🔀 Control Flow" colour="210">
          <block type="cobol_if" />
          <block type="cobol_condition" />
          <block type="cobol_evaluate" />
          <block type="cobol_when_case" />
          <block type="cobol_when_other" />
        </category>

        {/* LOOPS */}
        <category name="🔄 Loops" colour="230">
          <block type="cobol_perform" />
          <block type="cobol_perform_times" />
          <block type="cobol_perform_until" />
          <block type="cobol_perform_varying" />
          <block type="cobol_goto" />
        </category>

        {/* STRING MANIPULATION */}
        <category name="📝 String Operations" colour="65">
          <block type="cobol_string_concatenate" />
          <block type="cobol_unstring" />
          <block type="cobol_inspect" />
        </category>

        {/* INITIALIZATION & UTILITIES */}
        <category name="🔧 Initialization" colour="60">
          <block type="cobol_initialize" />
          <block type="cobol_initialize_multiple" />
          <block type="cobol_set" />
        </category>

        {/* PROGRAM CONTROL */}
        <category name="📞 Program Control" colour="100">
          <block type="cobol_call" />
          <block type="cobol_call_using" />
          <block type="cobol_exit" />
          <block type="cobol_exit_program" />
          <block type="cobol_stop_run" />
          <block type="cobol_return" />
        </category>

        {/* ERROR HANDLING */}
        <category name="⚠️ Error Handling" colour="300">
          <block type="cobol_on_size_error" />
          <block type="cobol_not_on_size_error" />
          <block type="cobol_invalid_key" />
        </category>

        {/* SORT & MERGE */}
        <category name="📋 Sort & Merge" colour="180">
          <block type="cobol_sort" />
          <block type="cobol_merge" />
        </category>

        {/* TABLE HANDLING */}
        <category name="📊 Table Operations" colour="270">
          <block type="cobol_search" />
          <block type="cobol_search_all" />
        </category>

        {/* REPORT WRITER */}
        <category name="📄 Report Writer" colour="320">
          <block type="cobol_report_section" />
          <block type="cobol_initiate" />
          <block type="cobol_generate" />
          <block type="cobol_terminate" />
        </category>

        {/* ADVANCED FEATURES */}
        <category name="🔬 Advanced Features" colour="350">
          <block type="cobol_copy" />
          <block type="cobol_replace" />
          <block type="cobol_exec_sql" />
          <block type="cobol_exec_cics" />
        </category>

        {/* COMPILER DIRECTIVES */}
        <category name="📋 Compiler Directives" colour="400">
          <block type="cobol_eject" />
          <block type="cobol_skip" />
          <block type="cobol_title" />
        </category>

        {/* QUICK START TEMPLATES */}
        <category name="🚀 Quick Start" colour="120">
          <label text="Basic Program Structure:" />
          <block type="cobol_identification_division">
            <field name="PROG">HELLO-WORLD</field>
            <field name="AUTHOR">Developer</field>
            <field name="INSTALLATION">Local</field>
            <next>
              <block type="cobol_environment_division">
                <next>
                  <block type="cobol_data_division">
                    <next>
                      <block type="cobol_working_storage_section">
                        <next>
                          <block type="cobol_variable_declaration">
                            <field name="LEVEL">01</field>
                            <field name="VAR">WS-MESSAGE</field>
                            <field name="PIC">X(50)</field>
                            <field name="VALUE">"Hello, World!"</field>
                            <next>
                              <block type="cobol_procedure_division">
                                <next>
                                  <block type="cobol_paragraph">
                                    <field name="PARA">MAIN-PARA</field>
                                    <statement name="BODY">
                                      <block type="cobol_display_variable">
                                        <field name="VAR">WS-MESSAGE</field>
                                        <next>
                                          <block type="cobol_stop_run" />
                                        </next>
                                      </block>
                                    </statement>
                                  </block>
                                </next>
                              </block>
                            </next>
                          </block>
                        </next>
                      </block>
                    </next>
                  </block>
                </next>
              </block>
            </next>
          </block>
          
          <label text="File Processing Template:" />
          <block type="cobol_identification_division">
            <field name="PROG">FILE-PROC</field>
            <field name="AUTHOR">Developer</field>
            <next>
              <block type="cobol_environment_division">
                <next>
                  <block type="cobol_input_output_section">
                    <next>
                      <block type="cobol_select_file">
                        <field name="SELECT">INPUT-FILE</field>
                        <field name="ASSIGN">input.txt</field>
                        <field name="ORG">LINE SEQUENTIAL</field>
                        <field name="ACCESS">SEQUENTIAL</field>
                        <next>
                          <block type="cobol_data_division">
                            <next>
                              <block type="cobol_file_section">
                                <next>
                                  <block type="cobol_fd_record">
                                    <field name="FD">INPUT-FILE</field>
                                    <field name="RECORD">INPUT-RECORD</field>
                                  </block>
                                </next>
                              </block>
                            </next>
                          </block>
                        </next>
                      </block>
                    </next>
                  </block>
                </next>
              </block>
            </next>
          </block>

          <label text="Loop Example:" />
          <block type="cobol_perform_varying">
            <field name="VAR">WS-COUNTER</field>
            <field name="FROM">1</field>
            <field name="BY">1</field>
            <field name="UNTIL">WS-COUNTER {'>'} 10</field>
            <statement name="DO">
              <block type="cobol_display_variable">
                <field name="VAR">WS-COUNTER</field>
                <next>
                  <block type="cobol_add">
                    <field name="NUM">1</field>
                    <field name="VAR">WS-TOTAL</field>
                  </block>
                </next>
              </block>
            </statement>
          </block>

          <label text="Conditional Processing:" />
          <block type="cobol_if">
            <value name="COND">
              <block type="cobol_condition">
                <field name="LEFT">WS-INPUT</field>
                <field name="OP">=</field>
                <field name="RIGHT">"Y"</field>
              </block>
            </value>
            <statement name="THEN">
              <block type="cobol_display">
                <field name="TEXT">Yes selected</field>
              </block>
            </statement>
            <statement name="ELSE">
              <block type="cobol_display">
                <field name="TEXT">No selected</field>
              </block>
            </statement>
          </block>
        </category>
      </xml>
      <div ref={blocklyDiv} style={{ height: '600px', width: '100%' }} />
    </>
  );
}