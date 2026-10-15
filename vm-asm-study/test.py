#!/usr/bin/env python3
"""Compile and execute focused VM regressions; leave no generated files in Git."""
import argparse
import os
from pathlib import Path
import re
import shutil
import struct
import subprocess
import sys
import tempfile

HERE = Path(__file__).resolve().parent


def executable(value):
    found = shutil.which(value)
    if not found:
        raise ValueError(f"Executable not found: {value}")
    return str(Path(found).resolve())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lua", default=os.environ.get("LUA", "lua"))
    parser.add_argument("--assembler", default=os.environ.get("TPTASM"), required="TPTASM" not in os.environ)
    parser.add_argument("--emulator", default=os.environ.get("R3EMU", "r3emu"))
    parser.add_argument("--compiler", default=str(HERE / "build/party_tpt"))
    args = parser.parse_args()
    lua, emulator = executable(args.lua), executable(args.emulator)
    assembler = str(Path(args.assembler).resolve())
    compiler = str(Path(args.compiler).resolve())
    # name, heap words, exact output, input, exit status, minimum collections
    cases = [
        ("hello", 768, "Hi\n", "", 0, 0),
        ("print123", 768, "123\n", "", 0, 1),
        ("branch", 768, "TF\n", "", 0, 0),
        ("arith_tests", 768, "T" * 14 + "FT\n", "", 0, 2),
        ("factorial", 768, "720\n", "", 0, 2),
        ("signed-edges", 1024, "T" * 13 + "\n", "", 0, 0),
        ("gc-loop", 512, "x" * 200 + "\n", "", 0, 10),
        ("keyboard", 512, "BA\n", "AB", 0, 0),
        ("references", 768, "AB\n", "", 0, 0),
        ("live-list", 512, "", "", 1, 1),
    ]
    with tempfile.TemporaryDirectory(prefix="r3-haskell-tests-") as tmp:
        for name, heap, output, keyboard, status, minimum_gc in cases:
            directory = Path(tmp) / name
            directory.mkdir()
            source = HERE / "tests" / f"{name}.hs"
            if not source.exists():
                source = HERE.parent / "examples" / f"{name}.hs"
            asm, binary, log = [directory / f"program.{ext}" for ext in ("asm", "bin", "log")]
            subprocess.run([
                sys.executable, str(HERE / "build_asm.py"), str(source),
                "--compiler", compiler, "--heap", str(heap), "--output", str(asm),
            ], check=True, capture_output=True)
            subprocess.run([
                lua, "-e", "unpack=unpack or table.unpack", assembler,
                str(asm), str(binary), str(log), "R3A4810",
            ], check=True, capture_output=True)
            assert "[EE]" not in log.read_text(), log.read_text()
            data = binary.read_bytes()
            assert len(data) % 4 == 0 and len(data) <= 6144 * 4, "ROM exceeds RAM"
            meta = struct.unpack("<7I", data[-28:])
            assert meta[0] == 0x13572468
            frames = []
            for _ in range(2):
                command = [
                    emulator, str(binary), "--profile", "showcase", "--memrows", "48",
                    "--headless", "--stdout", "--no-fpslimiter", "--max-cycles", "2000000",
                    "--memdump",
                ]
                if keyboard:
                    command += ["--keyboard-input", keyboard]
                run = subprocess.run(command, cwd=directory, capture_output=True, check=True, timeout=120)
                assert run.stdout == output.encode(), (name, run.stdout)
                halt = re.search(rb"stopped after (\d+) frames.*\(halted\)", run.stderr)
                assert halt, (name, "did not halt", run.stderr)
                frames.append(int(halt[1]))
                memory = [int(word) for word in (directory / "memdump.bin").read_text().split()]
                assert memory[meta[1]] & 65535 == status, (name, "VM exit status")
                assert memory[meta[2]] & 65535 >= minimum_gc, (name, "GC coverage")
                assert memory[meta[6]:meta[6] + 7] == list(meta), (name, "stack overrun")
            assert frames[0] == frames[1], (name, "nondeterministic frame count")
            print(f"PASS {name}: {frames[0]} frames, {len(data) // 4} words")
    print("All 10 cases passed twice.")


if __name__ == "__main__":
    main()
