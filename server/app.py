from flask import Flask, request, jsonify
from flask_cors import CORS
import subprocess
import tempfile
import uuid
import os

app = Flask(__name__)
CORS(app)

@app.route('/run', methods=['POST'])
def run_cobol():
    code = request.json.get('code', '').lstrip()
    if not code:
        return jsonify({'error': 'No COBOL code provided'}), 400

    # Build the temp filenames (short base name)
    tmp = tempfile.gettempdir()
    base = uuid.uuid4().hex[:6]
    src = os.path.join(tmp, f'{base}.cob')
    exe = os.path.join(tmp, f'{base}.out')

    # Write the user code to disk
    with open(src, 'w', encoding='utf-8') as f:
        f.write(code + '\n')

    # Compile in free format mode
    compile = subprocess.run(
        ['cobc', '-x', '-free', src, '-o', exe],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )

    if compile.returncode != 0:
        return jsonify({
            'status': 'compile_error',
            'stdout': compile.stdout.strip(),
            'stderr': compile.stderr.strip(),
            'src': code
        }), 400

    # Run the compiled program
    run = subprocess.run(
        [exe],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )

    if run.returncode != 0:
        return jsonify({
            'status': 'runtime_error',
            'stdout': run.stdout.strip(),
            'stderr': run.stderr.strip()
        }), 400

    return jsonify({
        'status': 'ok',
        'output': run.stdout
    })

if __name__ == '__main__':
    app.run(port=5000, debug=True)
