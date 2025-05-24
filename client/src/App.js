import { useState, useRef, useEffect } from 'react';
import BlocklyEditor from './BlocklyEditor';
import axios from 'axios';

function App() {
  const [code, setCode] = useState('');
  const [output, setOutput] = useState('');
  const [leftWidth, setLeftWidth] = useState(50); // Percentage
  const [isDragging, setIsDragging] = useState(false);
  const containerRef = useRef(null);

  const runCode = async () => {
    try {
      const res = await axios.post('http://localhost:5000/run', { code });
      setOutput(res.data.output);
    } catch (err) {
      setOutput('Error: ' + (err.response?.data?.error || err.message));
    }
  };

  const handleMouseDown = (e) => {
    setIsDragging(true);
    e.preventDefault();
  };

  const handleMouseMove = (e) => {
    if (!isDragging || !containerRef.current) return;
    
    e.preventDefault();
    const containerRect = containerRef.current.getBoundingClientRect();
    const newLeftWidth = ((e.clientX - containerRect.left) / containerRect.width) * 100;
    
    // Increased range: allow from 5% to 95% for more flexibility
    const constrainedWidth = Math.min(Math.max(newLeftWidth, 5), 95);
    setLeftWidth(constrainedWidth);
  };

  const handleMouseUp = () => {
    setIsDragging(false);
  };

  useEffect(() => {
    const handleGlobalMouseMove = (e) => {
      if (isDragging) {
        handleMouseMove(e);
      }
    };

    const handleGlobalMouseUp = () => {
      if (isDragging) {
        setIsDragging(false);
        document.body.style.cursor = '';
        document.body.style.userSelect = '';
      }
    };

    if (isDragging) {
      document.addEventListener('mousemove', handleGlobalMouseMove);
      document.addEventListener('mouseup', handleGlobalMouseUp);
      document.body.style.cursor = 'col-resize';
      document.body.style.userSelect = 'none';
    }

    return () => {
      document.removeEventListener('mousemove', handleGlobalMouseMove);
      document.removeEventListener('mouseup', handleGlobalMouseUp);
      if (!isDragging) {
        document.body.style.cursor = '';
        document.body.style.userSelect = '';
      }
    };
  }, [isDragging]);

  return (
    <div style={{ height: '100vh', display: 'flex', flexDirection: 'column' }}>
      <header style={{ 
        padding: '10px 20px', 
        backgroundColor: '#f5f5f5', 
        borderBottom: '1px solid #ddd',
        flexShrink: 0
      }}>
        <h1 style={{ margin: 0, fontSize: '24px', color: '#333' }}>COBOL Block Editor</h1>
      </header>
      
      <div 
        ref={containerRef}
        style={{ 
          display: 'flex', 
          flex: 1, 
          overflow: 'hidden',
          position: 'relative'
        }}
      >
        {/* Left Panel - Blockly Editor */}
        <div style={{ 
          width: `${leftWidth}%`, 
          height: '100%',
          overflow: 'hidden',
          backgroundColor: '#fff',
          minWidth: '150px',
          maxWidth: 'none' // Prevent it from being too wide
           // Ensure minimum width for usability
        }}>
          <div style={{ 
            padding: '10px', 
            height: '100%', 
            boxSizing: 'border-box',
            overflow: 'hidden', // Changed from 'auto' to 'hidden'
            display: 'flex',
            flexDirection: 'column'
          }}>
            <div style={{
              flex: 1,
              overflow: 'hidden'
            }}>
              <BlocklyEditor onCodeChange={setCode} />
            </div>
          </div>
        </div>

        {/* Resizer */}
        <div
          style={{
            width: '8px',
            backgroundColor: isDragging ? '#007acc' : '#ddd',
            cursor: 'col-resize',
            position: 'relative',
            flexShrink: 0,
            transition: isDragging ? 'none' : 'background-color 0.2s',
            borderLeft: '1px solid #ccc',
            borderRight: '1px solid #ccc'
          }}
          onMouseDown={handleMouseDown}
        >
          <div style={{
            position: 'absolute',
            top: '50%',
            left: '50%',
            transform: 'translate(-50%, -50%)',
            width: '24px',
            height: '50px',
            backgroundColor: isDragging ? '#007acc' : '#999',
            borderRadius: '12px',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            color: 'white',
            fontSize: '14px',
            transition: isDragging ? 'none' : 'background-color 0.2s',
            boxShadow: '0 2px 4px rgba(0,0,0,0.2)'
          }}>
            ⋮⋮
          </div>
        </div>

        {/* Right Panel - Code & Output */}
        <div style={{ 
          width: `${100 - leftWidth}%`, 
          height: '100%',
          display: 'flex',
          flexDirection: 'column',
          backgroundColor: '#f9f9f9'
        }}>
          {/* Generated Code Section */}
          <div style={{ 
            flex: '1 1 50%',
            display: 'flex',
            flexDirection: 'column',
            borderBottom: '1px solid #ddd',
            minHeight: '200px'
          }}>
            <div style={{ 
              padding: '10px 15px 5px', 
              backgroundColor: '#f0f0f0',
              borderBottom: '1px solid #ddd',
              flexShrink: 0
            }}>
              <div style={{ 
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'space-between',
                width: '100%'
              }}>
                <h3 style={{ 
                  margin: 0, 
                  fontSize: '16px', 
                  color: '#333'
                }}>
                  Generated COBOL Code
                </h3>
                <button 
                  onClick={runCode}
                  style={{
                    padding: '10px 20px',
                    backgroundColor: '#28a745',
                    color: 'white',
                    border: '2px solid #28a745',
                    borderRadius: '6px',
                    cursor: 'pointer',
                    fontSize: '14px',
                    fontWeight: 'bold',
                    boxShadow: '0 2px 6px rgba(40, 167, 69, 0.3)',
                    minWidth: '120px',
                    textTransform: 'uppercase',
                    letterSpacing: '0.5px'
                  }}
                  onMouseOver={(e) => {
                    e.target.style.backgroundColor = '#218838';
                    e.target.style.borderColor = '#218838';
                    e.target.style.transform = 'translateY(-1px)';
                    e.target.style.boxShadow = '0 4px 8px rgba(40, 167, 69, 0.4)';
                  }}
                  onMouseOut={(e) => {
                    e.target.style.backgroundColor = '#28a745';
                    e.target.style.borderColor = '#28a745';
                    e.target.style.transform = 'translateY(0)';
                    e.target.style.boxShadow = '0 2px 6px rgba(40, 167, 69, 0.3)';
                  }}
                >
                  ▶ Run COBOL
                </button>
              </div>
            </div>
            <div style={{ 
              flex: 1, 
              overflow: 'auto',
              padding: '10px'
            }}>
              <pre style={{ 
                margin: 0, 
                fontFamily: 'Consolas, Monaco, monospace',
                fontSize: '12px',
                lineHeight: '1.4',
                backgroundColor: '#fff',
                padding: '10px',
                border: '1px solid #ddd',
                borderRadius: '4px',
                minHeight: '100%',
                boxSizing: 'border-box',
                whiteSpace: 'pre-wrap',
                wordWrap: 'break-word'
              }}>
                {code || '// Generated COBOL code will appear here...'}
              </pre>
            </div>
          </div>

          {/* Output Section */}
          <div style={{ 
            flex: '1 1 50%',
            display: 'flex',
            flexDirection: 'column',
            minHeight: '200px'
          }}>
            <div style={{ 
              padding: '10px 15px 5px', 
              backgroundColor: '#f0f0f0',
              borderBottom: '1px solid #ddd',
              flexShrink: 0
            }}>
              <h3 style={{ 
                margin: 0, 
                fontSize: '16px', 
                color: '#333'
              }}>
                Output
              </h3>
            </div>
            <div style={{ 
              flex: 1, 
              overflow: 'auto',
              padding: '10px'
            }}>
              <pre style={{ 
                margin: 0, 
                fontFamily: 'Consolas, Monaco, monospace',
                fontSize: '12px',
                lineHeight: '1.4',
                backgroundColor: output.startsWith('Error:') ? '#ffe6e6' : '#fff',
                color: output.startsWith('Error:') ? '#d00' : '#000',
                padding: '10px',
                border: '1px solid #ddd',
                borderRadius: '4px',
                minHeight: '100%',
                boxSizing: 'border-box',
                whiteSpace: 'pre-wrap',
                wordWrap: 'break-word'
              }}>
                {output || '// Program output will appear here...'}
              </pre>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;