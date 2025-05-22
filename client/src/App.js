// === App.js ===
import { useState } from 'react'; 
import BlocklyEditor from './BlocklyEditor';
import axios from 'axios';

function App() {
  const [code, setCode] = useState('');
  const [output, setOutput] = useState('');

 const runCode = async () => {
  try {
    const res = await axios.post('http://localhost:5000/run', { code });
    setOutput(res.data.output);
  } catch (err) {
    setOutput('Error: ' + (err.response?.data?.error || err.message));
  }
};
  return (
    <div>
      <h1>COBOL Block Editor</h1>
      <BlocklyEditor onCodeChange={setCode} />
      <h3>Generated COBOL Code:</h3>
      <pre>{code}</pre>
      <button onClick={runCode}>Run COBOL</button>
      <h3>Output:</h3>
      <pre>{output}</pre>
    </div>
  );
} 
export default App;
