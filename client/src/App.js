import { useState, useRef, useEffect } from 'react'; 
import BlocklyEditor from './BlocklyEditor';
import axios from 'axios';

function App() {
  const [code, setCode] = useState('');
  const [output, setOutput] = useState('');
  const [leftWidth, setLeftWidth] = useState(70); 
  const [isDragging, setIsDragging] = useState(false);
  const containerRef = useRef(null);

  const runCode = async () => {
   try {
  const res = await axios.post('http://localhost:5000/run', { code });
  setOutput(res.data.output);
} catch (err) {
  const e = err.response?.data;
  setOutput(`ERROR (${e?.status || 'unknown'}):\n${e?.message || ''}\n\nDetails:\n${e?.details || e?.stderr || err.message}`);
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
    const constrainedWidth = Math.min(Math.max(newLeftWidth, 5), 95);
    setLeftWidth(constrainedWidth);
  };

  const handleMouseUp = () => {
    setIsDragging(false);
  };

  useEffect(() => {
    const handleGlobalMouseMove = (e) => {
      if (isDragging) handleMouseMove(e);
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
    <div style={{ height: '100vh', display: 'flex', flexDirection: 'column', background: '#1e1e2f', color: '#fff' }}>
      <header style={{ 
        padding: '10px 20px', 
        background: 'linear-gradient(90deg, #2c2f4a, #434675)', 
        borderBottom: '2px solid #333',
        flexShrink: 0,
        boxShadow: '0 4px 10px rgba(0, 0, 0, 0.4)'
      }}>
        <h1 style={{ margin: 0, fontSize: '24px', color: '#ffffff' }}>🚀 COBOL Block Editor</h1>
      </header>
      
      <div 
        ref={containerRef}
        style={{ display: 'flex', flex: 1, overflow: 'hidden', position: 'relative' }}
      >
        {/* Left Panel */}
        <div style={{ 
          width: `${leftWidth}%`, 
          height: '100%',
          overflow: 'hidden',
          backgroundColor: '#2d2d3a',
          minWidth: '150px',
          display: 'flex',
          flexDirection: 'column',
          boxShadow: 'inset -3px 0 10px rgba(0,0,0,0.4)'
        }}>
          <div style={{ 
            padding: '10px', 
            height: '100%', 
            width: '100%',
            boxSizing: 'border-box',
            overflow: 'hidden', 
            display: 'flex',
            flexDirection: 'column'
          }}>
            <div style={{ flex: 1, overflow: 'hidden' }}>
              <BlocklyEditor onCodeChange={setCode} />
            </div>
          </div>
        </div>

        {/* Resizer */}
        <div
          style={{
            width: '8px',
            backgroundColor: isDragging ? '#00c6ff' : '#444',
            cursor: 'col-resize',
            position: 'relative',
            flexShrink: 0,
            borderLeft: '1px solid #000',
            borderRight: '1px solid #000'
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
            backgroundColor: isDragging ? '#00c6ff' : '#666',
            borderRadius: '12px',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            color: 'white',
            fontSize: '14px',
            boxShadow: '0 2px 4px rgba(0,0,0,0.5)'
          }}>
            ⋮⋮
          </div>
        </div>

        {/* Right Panel */}
        <div style={{ 
          width: `${100 - leftWidth}%`, 
          height: '100%',
          display: 'flex',
          flexDirection: 'column',
          backgroundColor: '#1a1a26'
        }}>
          {/* Code Display */}
          {/* Code Display */}
<div style={{ 
  flex: '1 1 50%',
  display: 'flex',
  flexDirection: 'column',
  borderBottom: '1px solid #555',
  overflow: 'hidden' //important for nested scroll
}}>
  <div style={{ 
    padding: '10px 15px 5px', 
    backgroundColor: '#262635',
    borderBottom: '1px solid #555',
    flexShrink: 0
  }}>
    <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between' }}>
      <h3 style={{ margin: 0, fontSize: '16px', color: '#88f2ff' }}>Generated COBOL Code</h3>
      <button 
        onClick={runCode}
        style={{
          padding: '10px 20px',
          backgroundColor: '#00c853',
          color: '#fff',
          border: '2px solid #00c853',
          borderRadius: '6px',
          cursor: 'pointer',
          fontSize: '14px',
          fontWeight: 'bold',
          boxShadow: '0 2px 8px rgba(0, 200, 83, 0.4)',
          transition: 'all 0.2s ease-in-out',
          textTransform: 'uppercase'
        }}
        onMouseOver={(e) => {
          e.target.style.backgroundColor = '#00e676';
          e.target.style.borderColor = '#00e676';
        }}
        onMouseOut={(e) => {
          e.target.style.backgroundColor = '#00c853';
          e.target.style.borderColor = '#00c853';
        }}
      >
        ▶ Run COBOL
      </button>
    </div>
  </div>
  <div style={{ flex: 1, overflow: 'auto', padding: '10px' }}>
    <pre style={{ 
      margin: 0, 
      fontFamily: 'Consolas, Monaco, monospace',
      fontSize: '13px',
      lineHeight: '1.5',
      backgroundColor: '#0d0d1a',
      color: '#f8f8f2',
      padding: '10px',
      border: '1px solid #444',
      borderRadius: '6px',
      boxSizing: 'border-box',
      whiteSpace: 'pre-wrap',
      overflow: 'auto',
      maxHeight: '100%'
    }}>
      {code || '// Generated COBOL code will appear here...'}
    </pre>
  </div>
</div>
          {/* Output Display */}
<div style={{ 
  flex: '1 1 50%',
  display: 'flex',
  flexDirection: 'column',
  overflow: 'hidden' // allow internal scroll
}}>
  <div style={{ 
    padding: '10px 15px 5px', 
    backgroundColor: '#262635',
    borderBottom: '1px solid #555',
    flexShrink: 0
  }}>
    <h3 style={{ margin: 0, fontSize: '16px', color: '#fbc02d' }}>Output</h3>
  </div>
  <div style={{ flex: 1, overflow: 'auto', padding: '10px' }}>
    <pre style={{ 
      margin: 0, 
      fontFamily: 'Consolas, Monaco, monospace',
      fontSize: '13px',
      lineHeight: '1.5',
      backgroundColor: output.startsWith('Error:') ? '#2d0b0b' : '#0d0d1a',
      color: output.startsWith('Error:') ? '#ff5252' : '#e0e0e0',
      padding: '10px',
      border: '1px solid #444',
      borderRadius: '6px',
      boxSizing: 'border-box',
      whiteSpace: 'pre-wrap',
      overflow: 'auto',
      maxHeight: '100%'
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
