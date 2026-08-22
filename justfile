scrollback_leak_benchmark:
    ./tools/bench/scrollback_leak_benchmark
    @echo -e "\n>>> Open $(pwd)/hat-server.eventlog.html"

reload_rehydrate_profile blob='/tmp/hat-1000/default.reload.last':
    ./tools/bench/reload_rehydrate_profile {{blob}}
    @echo -e "\n>>> Open $(pwd)/reload-rehydrate.eventlog.html"

# Time-profile the hat binary under an ad-hoc workload
time_profile +ARGS:
    #!/usr/bin/env bash
    # e.g. just time_profile -S /tmp/hat-1000/default ls-sessions
    # Its own dist dir keeps the ordinary build, and cabal test, unprofiled.
    set -euo pipefail
    cabal build --enable-profiling --profiling-detail=late --builddir=dist-prof exe:hat
    bin=$(cabal list-bin --builddir=dist-prof exe:hat)
    # A profiled workload may exit non-zero (a killed server does); the
    # profile is still written and is still what we came for.
    GHCRTS="-p -po$PWD/hat-time" "$bin" {{ARGS}} || echo "(workload exited $?)"
    sed -n '1,30p' hat-time.prof
    echo -e "\n>>> Full profile in $PWD/hat-time.prof"

# Path to the profiled binary, to drive by hand
profiled_hat:
    @cabal build --enable-profiling --profiling-detail=late --builddir=dist-prof exe:hat >/dev/null
    @cabal list-bin --builddir=dist-prof exe:hat

# Memory benchmark: fixed workload; reload/keep take 'yes', build/rts take flags
mem_bench lines='20000' reload='no' build='' rts='' keep='no':
    #!/usr/bin/env bash
    set -Eeuo pipefail
    args=(--lines '{{lines}}')
    if [ '{{reload}}' = yes ]; then args+=(--reload); fi
    if [ '{{keep}}' = yes ]; then args+=(--keep); fi
    if [ -n '{{build}}' ]; then args+=(--build '{{build}}'); fi
    if [ -n '{{rts}}' ]; then args+=(--rts '{{rts}}'); fi
    ./tools/bench/hat_mem "${args[@]}"

ghcid:
    ghcid -c 'cabal repl --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j --enable-multi-repl --enable-tests --enable-benchmarks all' -o errors.err --restart hat.cabal --restart src/Hat/Term/Emulator.hsc
