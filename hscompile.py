#!/usr/bin/env python3
"""
hscompile.py - Haskell to TPT-C Compiler Wrapper

Uses the blynn-compiler (party_tpt) to compile Haskell to C directly.

Usage: python3 hscompile.py input.hs [output.c]
"""

import os
import subprocess
import sys

COMPILER_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "compiler")
COMPILER_PATH = os.path.join(COMPILER_DIR, "party_tpt")


def run_compiler(source: str) -> str:
    result = subprocess.run(
        [COMPILER_PATH],
        input=source,
        capture_output=True,
        text=True,
    )

    if result.returncode != 0:
        raise RuntimeError(result.stderr.strip() or "Compilation failed")

    return result.stdout


def main() -> int:
    if len(sys.argv) < 2:
        print("Usage: python3 hscompile.py input.hs [output.c]")
        return 1

    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else input_file.rsplit(".", 1)[0] + ".c"

    if not os.path.exists(COMPILER_PATH):
        print(f"Error: compiler not found at {COMPILER_PATH}")
        print("Please rebuild the compiler in compiler/ first.")
        return 1

    with open(input_file, "r") as f:
        source = f.read()

    try:
        c_code = run_compiler(source)
    except RuntimeError as exc:
        print(f"Error: {exc}")
        return 1

    with open(output_file, "w") as f:
        f.write(c_code)

    print(f"Compiled {input_file} -> {output_file}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
