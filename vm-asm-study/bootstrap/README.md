# Compiler bootstrap sources

Copied from the local Blynn compiler checkout for reproducibility, without publishing that checkout's binaries or unrelated experiments.

- Upstream: https://github.com/blynn/compiler
- Local checkout HEAD: c37cf9c48b33cb71d9596f16e7b878d066782092
- `methodically.c`: standalone bootstrap compiler; rebuild with a C compiler.
- `party_tpt.hs`: local TPT-target adaptation used in this study; not claimed to be pristine upstream.
- `LICENSE`: upstream license, retained verbatim.

These copies are byte-identical to the files used in the recorded compiler rebuild. See the parent README for build commands. The original nested `compiler/` checkout is not changed or committed by this publication.
