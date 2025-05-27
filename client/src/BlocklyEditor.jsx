import * as Blockly from 'blockly';
import 'blockly/blocks';
import './BlocklyEditor.css';
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
 categoryFlyoutItemDefaults: {
    showIcon: true,       
    rowClassName: 'blocklyTreeRow',
   
  },
  sounds: true,
  oneBasedIndex: true,
   move: {
    scrollbars: true,
    drag: true, 
    wheel: true
  },
  grid: {
    spacing: 25,
    length: 3,
    colour: '#ccc',
    snap: true
  },
  zoom: {
    controls: true,
    wheel: true,
    startScale: 0.9,
    maxScale: 3,
    minScale: 0.3,
    scaleSpeed: 1.2
  }
});
// immediately after workspace = Blockly.inject(...)
const applyCategoryColors = () => {
  const toolbox = workspace.getToolbox?.();
  if (!toolbox) return;
  const cats = toolbox.getToolboxItems?.() || [];
  document.querySelectorAll('.blocklyTreeRow').forEach(row => {
    const label = row.querySelector('.blocklyTreeLabel')?.textContent?.trim();
    const cat = cats.find(c => c.name_ === label);
    const btn = row.querySelector('.blocklyTreeButton');
    if (cat && btn) {
      btn.style.background = `#${cat.colour_}`;
      btn.style.borderColor = shade(cat.colour_, -20);
    }
  });
};
setTimeout(applyCategoryColors, 200);



const toggleButton = document.getElementById('toolboxToggleBtn');
const toolboxEl = document.querySelector('.blocklyToolbox');

toggleButton?.addEventListener('click', () => {
  toolboxEl?.classList.toggle('collapsed');
  toggleButton.textContent = toolboxEl?.classList.contains('collapsed') ? '➡️' : '⬅️';
});

// helper to darken/lighten hex
function shade(hex, percent) {
  const num = parseInt(hex,16),
        amt = Math.round(2.55 * percent),
        R = (num >> 16) + amt,
        G = ((num >> 8) & 0x00ff) + amt,
        B = (num & 0x0000ff) + amt;
  return (
    '#' +
    (
      0x1000000 +
      (R<255?R<1?0:R:255)*0x10000 +
      (G<255?G<1?0:G:255)*0x100 +
      (B<255?B<1?0:B:255)
    )
      .toString(16)
      .slice(1)
  );
}

    workspaceRef.current = workspace;

    workspace.addChangeListener(() => {
      const topBlocks = workspace.getTopBlocks(true);

    // 2. Generate code for each block and join
   let fullCode = '';
topBlocks.forEach(block => {
    const code = cobolGenerator.blockToCode(block);
      fullCode += Array.isArray(code) ? code[0] : code;
   });
  // 3. Push the concatenated code back up
   onCodeChange(fullCode);
      
    });

    return () => {
      if (workspaceRef.current) {
        workspaceRef.current.dispose();
      }
    };
  }, [onCodeChange]);

  return (
<div style={{ height: '100%', width: '100%' }}>
      <xml id="toolbox" style={{ display: 'none' }}>
        {/*IDENTIFICATION DIVISION */}
        <category name="📋Program" >
            <label text="🪪 Identification Division" />
          <block type="cobol_identification_division" />
          <block type="cobol_remarks" />
          
  <label text="🌐Environment Division" />
   <block type="cobol_environment_division">

       <next>
    <block type="cobol_configuration_section">
      <next>
        <block type="cobol_input_output_section">
          <next>
            <block type="cobol_select_file"></block>
          </next>
        </block>
      </next>
    </block>
  </next>
</block>

 <block type="cobol_environment_division"></block>
  <block type="cobol_configuration_section"></block>
  <block type="cobol_input_output_section"></block>
  <block type="cobol_select_file"></block>
  
        </category>       
       <category name="📊Data" >

  <label text="📄 Data Division" />
  <block type="cobol_data_division">
   
            <next>
              <block type="cobol_working_storage_section">
                <next>
                  <block type="cobol_variable_declaration">
                    <next>
                      <block type="cobol_group_item">
                        <next>
                          <block type="cobol_occurs_clause">
                            <next>
                              <block type="cobol_linkage_section"></block>
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
        

  <label text="📁 File & Working Storage" />
  <block type="cobol_data_division" />
  <block type="cobol_file_section" />
  <block type="cobol_fd_record" />
  <block type="cobol_working_storage_section" />
  <label text="📦 Variables & Group Items" />
  <block type="cobol_variable_declaration" />
  <block type="cobol_group_item" />
  <block type="cobol_occurs_clause" />
  <label text="🔗 Linkage" />
  <block type="cobol_linkage_section" />
</category>
        {/*PROCEDURE DIVISION*/}
          <category name="🔀Logic" >
          <label text="🔧 Procedure Division" />

          <block type="cobol_procedure_division" />
          <block type="cobol_procedure_using" />
          <block type="cobol_paragraph" />
          <block type="cobol_section" />
          
  <label text="🧹 Initialization" />
  <block type="cobol_initialize" />
  <block type="cobol_initialize_multiple" />
  <block type="cobol_set" />

  <label text="🧭 Control Flow" />
  <block type="cobol_if" />
  <block type="cobol_condition" />
  <block type="cobol_evaluate" />
  <block type="cobol_when_case" />
  <block type="cobol_when_other" />

  <label text="🔁 Loops" />
  <block type="cobol_perform" />
  <block type="cobol_perform_times" />
  <block type="cobol_perform_until" />
  <block type="cobol_perform_varying" />
  <block type="cobol_goto" />

  <label text="🛠️ Program Control" />
  <block type="cobol_call" />
  <block type="cobol_call_using" />
  <block type="cobol_exit" />
  <block type="cobol_exit_program" />
  <block type="cobol_stop_run" />
  <block type="cobol_return" />


</category>


        {/* INPUT/OUTPUT */}
        <category name="📁File & I/O" >
        
          <label text="📺I/O" />
          <block type="cobol_display" />
          <block type="cobol_display_variable" />
          <block type="cobol_accept" />
          <block type="cobol_accept_date" />
            <label text="📁File" />

           <block type="cobol_open" />

          <block type="cobol_close" />
          <block type="cobol_read" />
          <block type="cobol_write" />
          <block type="cobol_rewrite" />
          <block type="cobol_delete" />
        </category>

        

        {/* ARITHMETIC OPERATIONS */}
        <category name="🔢Arithmetic" >
          <label text="🧮 Arithmetic" />
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
                    <label text=" 🔤Strings" />

          <block type="cobol_string_concatenate" />
          <block type="cobol_unstring" />
          <block type="cobol_inspect" />
        </category>
        {/* ERROR HANDLING */}
      <category name="🧩 Utilities" >
  <label text="⚠️ Error Handling" />
  <block type="cobol_on_size_error" />
  <block type="cobol_not_on_size_error" />
  <block type="cobol_invalid_key" />

  <label text="📋 Sort & Merge" />
  <block type="cobol_sort" />
  <block type="cobol_merge" />

  <label text="📊 Table Operations" />
  <block type="cobol_search" />
  <block type="cobol_search_all" />

  <label text="📄 Report Writer" />
  <block type="cobol_report_section" />
  <block type="cobol_initiate" />
  <block type="cobol_generate" />
  <block type="cobol_terminate" />

  <label text="🔬 Advanced Features" />
  <block type="cobol_copy" />
  <block type="cobol_replace" />
  <block type="cobol_exec_sql" />
  <block type="cobol_exec_cics" />

  <label text="🛠️ Compiler Directives" />
  <block type="cobol_eject" />
  <block type="cobol_skip" />
  <block type="cobol_title" />
</category>


        {/* QUICK START TEMPLATES */}
        <category name="🚀Quick" >
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
        <category name="📚Sample" >
  <label text="Addition of Two Numbers:" />
  <block type="cobol_identification_division">
    <field name="PROG">HELLO-WORLD</field>
    <field name="AUTHOR">Developer</field>
    <field name="INSTALLATION">Local</field>
    <field name="WRITTEN">2025-05-20</field>
    <field name="COMPILED">AUTO</field>
    <field name="SECURITY">NONE</field>
    <next>
      <block type="cobol_environment_division">
        <next>
          <block type="cobol_data_division">
            <next>
              <block type="cobol_working_storage_section">
                <next>
                  <block type="cobol_variable_declaration">
                    <field name="LEVEL">01</field>
                    <field name="VAR">var1</field>
                    <field name="PIC">9(4)</field>
                    <field name="VALUE">10</field>
                    <next>
                      <block type="cobol_variable_declaration">
                        <field name="LEVEL">01</field>
                        <field name="VAR">var2</field>
                        <field name="PIC">9(4)</field>
                        <field name="VALUE">20</field>
                        <next>
                          <block type="cobol_variable_declaration">
                            <field name="LEVEL">01</field>
                            <field name="VAR">var3</field>
                            <field name="PIC">9(4)</field>
                            <field name="VALUE"></field>
                            <next>
                              <block type="cobol_procedure_division">
                                <next>
                                  <block type="cobol_paragraph">
                                    <field name="PARA">MAIN-PARA</field>
                                    <statement name="BODY">
                                      <block type="cobol_add_giving">
                                        <field name="A">var1</field>
                                        <field name="B">var2</field>
                                        <field name="RESULT">var3</field>
                                        <next>
                                          <block type="cobol_display_variable">
                                            <field name="VAR">var3</field>
                                            <next>
                                              <block type="cobol_stop_run" />
                                            </next>
                                          </block>
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
        </next>
      </block>
    </next>
  </block>
  
<label text="Check Which Number is Greater:" />
<block type="cobol_identification_division">
  <field name="PROG">COMPARE-NUM</field>
  <field name="AUTHOR">Dev</field>
  <next>
    <block type="cobol_data_division">
      <next>
        <block type="cobol_working_storage_section">
          <next>
            <block type="cobol_variable_declaration">
              <field name="LEVEL">01</field>
              <field name="VAR">X</field>
              <field name="PIC">9(3)</field>
              <field name="VALUE">15</field>
              <next>
                <block type="cobol_variable_declaration">
                  <field name="LEVEL">01</field>
                  <field name="VAR">Y</field>
                  <field name="PIC">9(3)</field>
                  <field name="VALUE">10</field>
                  <next>
                    <block type="cobol_procedure_division">
                      <next>
                        <block type="cobol_paragraph">
                          <field name="PARA">MAIN-PARA</field>
                          <statement name="BODY">
                            <block type="cobol_if">
                              <value name="COND">
                                <block type="cobol_condition">
                                  <field name="LEFT">X</field>
                                  <field name="OP">&gt;</field>
                                  <field name="RIGHT">Y</field>
                                </block>
                              </value>
                              <statement name="THEN">
                                <block type="cobolif_display">
                                  <field name="TEXT">X is greater</field>
                                </block>
                              </statement>
                              <statement name="ELSE">
                                <block type="cobolif_display">
                                  <field name="TEXT">Y is greater</field>
                                </block>
                              </statement>
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

<label text="Loop from 1 to 5:" />
<block type="cobol_identification_division">
  <field name="PROG">LOOP-TEST</field>
  <field name="AUTHOR">Dev</field>
  <next>
    <block type="cobol_data_division">
      <next>
        <block type="cobol_working_storage_section">
          <next>
            <block type="cobol_variable_declaration">
              <field name="LEVEL">01</field>
              <field name="VAR">COUNTER</field>
              <field name="PIC">9(1)</field>
              <field name="VALUE">0</field>
              <next>
                <block type="cobol_procedure_division">
                  <next>
                    <block type="cobol_paragraph">
                      <field name="PARA">MAIN-PARA</field>
                      <statement name="BODY">
                        <block type="cobol_perform_varying">
                          <field name="VAR">COUNTER</field>
                          <field name="FROM">1</field>
                          <field name="BY">1</field>
                          <field name="UNTIL">COUNTER &gt; 5</field>
                          <statement name="DO">
                            <block type="cobolif_display">
                              <field name="VAR">COUNTER</field>
                            </block>
                          </statement>
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
<label text="Multiply Two Numbers:" />
<block type="cobol_identification_division">
  <field name="PROG">MULTIPLY-TWO</field>
  <field name="AUTHOR">Dev</field>
  <next>
    <block type="cobol_data_division">
      <next>
        <block type="cobol_working_storage_section">
          <next>
            <block type="cobol_variable_declaration">
              <field name="LEVEL">01</field>
              <field name="VAR">A</field>
              <field name="PIC">9(3)</field>
              <field name="VALUE">5</field>
              <next>
                <block type="cobol_variable_declaration">
                  <field name="LEVEL">01</field>
                  <field name="VAR">B</field>
                  <field name="PIC">9(3)</field>
                  <field name="VALUE">4</field>
                  <next>
                    <block type="cobol_variable_declaration">
                      <field name="LEVEL">01</field>
                      <field name="VAR">RESULT</field>
                      <field name="PIC">9(4)</field>
                      <next>
                        <block type="cobol_procedure_division">
                          <next>
                            <block type="cobol_paragraph">
                              <field name="PARA">MAIN-PARA</field>
                              <statement name="BODY">
                                <block type="cobol_compute">
                                  <field name="RESULT">RESULT</field>
                                  <field name="EXPRESSION">A * B</field>
                                  <next>
                                    <block type="cobol_display_variable">
                                      <field name="VAR">RESULT</field>
                                      <next>
                                        <block type="cobol_stop_run" />
                                      </next>
                                    </block>
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
  </next>
</block>

</category>

      </xml>
       <div
      ref={blocklyDiv}
      className="dark-blockly-container"
       style={{
          height: '100%',
          width: '100%',
          position: 'relative',
          overflow: 'hidden'
        }}
    />
    
    </div>
    
  );
}