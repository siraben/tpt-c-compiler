#!/usr/bin/env python3
"""Extract Blynn's compiled graph; link it to the hand-written R3 reducer.
No Haskell parsing or combinator compilation is performed in Python.
"""
import argparse, hashlib, json, pathlib, re, subprocess
ROOT = pathlib.Path(__file__).resolve().parent.parent
p = argparse.ArgumentParser()
p.add_argument('source', type=pathlib.Path)
p.add_argument('--heap', type=int, default=768)
p.add_argument('--compiler', type=pathlib.Path,
               default=ROOT/'vm-asm-study/build/party_tpt')
p.add_argument('--output', type=pathlib.Path, required=True)
a = p.parse_args()
a.output.parent.mkdir(parents=True, exist_ok=True)
c = subprocess.check_output([str(a.compiler.resolve()), '--heap', str(a.heap)], input=a.source.read_bytes(), cwd=ROOT).decode()
a.output.with_suffix('.c').write_text(c)
def array(name):
    m = re.search(r'\bu '+name+r'\[\]=\{([^}]+)\};', c)
    if not m:
        raise ValueError('missing compiler array: '+name)
    return [int(v) for v in m[1].split(',') if v.strip()]
graph, roots = array('prog'), array('root')
entry = int(re.search(r'int main\(\)\{rts_reduce\((\d+)\);return 0;\}', c)[1])
if len(graph)+128+16 >= a.heap:
    raise ValueError('insufficient initial heap headroom')
foreign = c.split('static void foreign(u n) {')[1].split('int run()')[0]
entries = []
for index, body in sorted(re.findall(r'case (\d+): \{(.*?)\}', foreign), key=lambda item: int(item[0])):
    if int(index) != len(entries):
        raise ValueError('non-contiguous foreign table')
    if re.fullmatch(r'u r = putchar\(num\(2\)\); lazy2\(4, app\(arg\(4\), app\(_NUM, r\)\), arg\(3\)\); break;', body):
        entries.append('ffi_putchar')
    elif re.fullmatch(r'u r = getchar\(\); lazy2\(3, app\(arg\(3\), app\(_NUM, r\)\), arg\(2\)\); break;', body):
        entries.append('ffi_getchar')
    else:
        raise ValueError('unsupported FFI body: '+body)
def words(values):
    return '\n'.join('    dw '+','.join(map(str,values[i:i+16])) for i in range(0,len(values),16))+'\n'
text = '%include "common"\n'+f'%define TOP {a.heap}\n%define ENTRY {entry}\n%define GRAPH_WORDS {len(graph)}\n%define FFI_COUNT {len(entries)}\n'
text += (ROOT/'vm-asm-study/runtime.asm').read_text()
text += '\nffi_table:\n    dw '+','.join(entries or ['bad_foreign'])+'\nroot_table:\n'+words(roots)
text += 'heap_a:\n'+words([0]*128+graph+[0]*(a.heap-128-len(graph)))
text += 'heap_b:\n'+words([0]*a.heap)
text += 'evac_stack:\n'+words([0]*(a.heap+32))+'evac_stack_end:\nimage_end:\n'
text += '    dw 0x13572468,exit_status,gc_count,heap_a,heap_b,evac_stack,image_end\n'
a.output.write_text(text)
a.output.with_suffix('.json').write_text(json.dumps({'graph_words':len(graph),'entry':entry,'heap_words':a.heap,'ffi':entries,'graph_sha256':hashlib.sha256(json.dumps(graph).encode()).hexdigest(),'source_sha256':hashlib.sha256(a.source.read_bytes()).hexdigest()},indent=2)+'\n')
print(a.output)
