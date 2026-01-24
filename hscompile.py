#!/usr/bin/env python3
"""
hscompile.py - Haskell to TPT-C Compiler Wrapper

Uses the blynn-compiler (party_tpt) to compile Haskell to C directly.

Usage: python3 hscompile.py [party_tpt options] input.hs [output.c]
Options (passed through to party_tpt):
  comb | lamb | type
  --heap N
  --top N
"""

import os
import subprocess
import sys

COMPILER_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "compiler")
COMPILER_PATH = os.path.join(COMPILER_DIR, "party_tpt")

USAGE = (
    "Usage: python3 hscompile.py [party_tpt options] input.hs [output.c]\n"
    "Options (passed through to party_tpt): comb | lamb | type | --heap N | --top N"
)

def run_compiler(source: str, compiler_args: list[str]) -> str:
    result = subprocess.run(
        [COMPILER_PATH] + compiler_args,
        input=source,
        capture_output=True,
        text=True,
    )

    if result.returncode != 0:
        raise RuntimeError(result.stderr.strip() or "Compilation failed")

    return result.stdout


def main() -> int:
    if len(sys.argv) < 2:
        print(USAGE)
        return 1

    if any(arg in ("-h", "--help") for arg in sys.argv[1:]):
        print(USAGE)
        return 0

    compiler_args = []
    input_file = None
    output_file = None
    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]
        if arg in ("comb", "lamb", "type"):
            compiler_args.append(arg)
            i += 1
            continue
        if arg in ("--heap", "--top"):
            if i + 1 >= len(sys.argv):
                print(f"Error: missing value for {arg}")
                return 1
            compiler_args.extend([arg, sys.argv[i + 1]])
            i += 2
            continue
        if input_file is None:
            input_file = arg
            i += 1
            continue
        if output_file is None:
            output_file = arg
            i += 1
            continue
        print(f"Error: unexpected argument {arg}")
        return 1

    if input_file is None:
        print(USAGE)
        return 1
    if output_file is None:
        output_file = input_file.rsplit(".", 1)[0] + ".c"

    if not os.path.exists(COMPILER_PATH):
        print(f"Error: compiler not found at {COMPILER_PATH}")
        print("Please rebuild the compiler in compiler/ first.")
        return 1

    with open(input_file, "r") as f:
        source = f.read()

    try:
        c_code = run_compiler(source, compiler_args)
    except RuntimeError as exc:
        print(f"Error: {exc}")
        return 1

    with open(output_file, "w") as f:
        f.write(c_code)

    print(f"Compiled {input_file} -> {output_file}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
