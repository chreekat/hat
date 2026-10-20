#!/usr/bin/env bash
# Run tmux's regress/ suite against hat, in parallel.
#
# Usage: tools/run-upstream-tests.sh /path/to/tmux-source [-j N] [test.sh ...]
#
# Prints each script's outcome and wall-clock seconds, slowest first. Set
# HAT_UPSTREAM_LOGS=DIR to keep each script's combined output as DIR/<name>.log.
# Scripts listed in tools/upstream-serial.txt run after the parallel batch,
# one at a time, and are marked (serial) in the report.
#
# Outcomes per script (see ARCHITECTURE.md "Testing strategy"):
#   PASS  - exit 0, not in xfail list
#   XFAIL - nonzero, listed in tools/upstream-xfail.txt   -> suite ok
#   XPASS - exit 0 but listed in xfail                    -> suite FAILS
#   FAIL  - nonzero, not listed                           -> suite FAILS
#
# tmux's regress dir is ISC-licensed and not vendored here; point this
# at a checkout (e.g. ~/src/tmux).
set -u

# Default to the nix-pinned tmux source ($HAT_TMUX_SRC, exported by the dev
# shell) so no path need be passed; a first arg that is a tmux checkout (has a
# regress/ dir) still overrides it, while bare test names fall through to it.
if [ $# -gt 0 ] && [ -d "$1/regress" ]; then
    tmux_src=$1
    shift
else
    tmux_src=${HAT_TMUX_SRC:?usage: run-upstream-tests.sh /path/to/tmux-source [-j N] [tests...] (or enter the dev shell for $HAT_TMUX_SRC)}
fi

cores=$(nproc 2>/dev/null || echo 4)
jobs=$cores
if [ "${1:-}" = "-j" ]; then
    jobs=$2
    shift 2
fi

regress="$tmux_src/regress"
[ -d "$regress" ] || { echo "no regress dir at $regress" >&2; exit 2; }

here=$(dirname "$0")
xfail_file="$here/upstream-xfail.txt"
serial_file="$here/upstream-serial.txt"
# $HAT_BIN overrides; else build and locate the dev binary as one step, so it
# is fresh and the two agree on a builddir. Never fall through to a $PATH
# `hat`, which on a dev box is the stale system-installed one.
if [ -z "${HAT_BIN:-}" ]; then
    cabal build -v0 exe:hat || { echo "hat build failed" >&2; exit 2; }
fi
hat_bin=${HAT_BIN:-$(cabal list-bin hat 2>/dev/null)} \
    || { echo "build hat first" >&2; exit 2; }
[ -x "$hat_bin" ] || { echo "build hat first (no binary at: $hat_bin)" >&2; exit 2; }

if [ $# -gt 0 ]; then
    tests=$(printf '%s\n' "$@" | xargs -n1 basename)
else
    tests=$(cd "$regress" && ls *.sh)
fi

# The scripts named in a list file, comments and reasons stripped.
listed_in() {
    [ -f "$1" ] && sed 's/#.*//' "$1" | awk 'NF { print $1 }'
}
serial_names=$(listed_in "$serial_file")
is_serial() { printf '%s\n' $serial_names | grep -qxF "$1"; }
parallel_tests=""; serial_tests=""
for t in $tests; do
    if is_serial "$t"; then serial_tests="$serial_tests $t"
    else parallel_tests="$parallel_tests $t"; fi
done

work=$(mktemp -d)
trap 'rm -rf "$work"' 0 1 15
results="$work/results"
: > "$results"

# Each test runs in isolation: private TMUX_TMPDIR so sockets don't
# collide, private HOME so any ~/.config lookups are neutral, and
# HAT_PERSIST=0 so hat's session persistence (a hat-only feature that
# changes server-restart behaviour) can't skew tmux's own expectations.
# Upstream scripts hardcode PATH=/bin:/usr/bin — strip that line so the
# script inherits our devShell PATH.
run_one() {
    name=$1
    tdir=$(mktemp -d)
    # Teardown is the trap's job, not the script's: every server spawned in
    # here carries this private $tdir in its argv, so the pattern kill reaps
    # exactly our servers and can never match anyone else's.
    reap() {
        pkill -TERM -f -- "--server $tdir/" 2>/dev/null
        for _ in $(seq 20); do
            pgrep -f -- "--server $tdir/" >/dev/null || break
            sleep 0.1
        done
        pkill -KILL -f -- "--server $tdir/" 2>/dev/null
    }
    trap 'reap; rm -rf "$tdir"' EXIT
    # Copy the script and its shared includes, stripping the hardcoded
    # PATH=/bin:/usr/bin (no coreutils there on NixOS) so both inherit the
    # devShell PATH. Includes are sourced cwd-relative (. ./input-common.inc),
    # so run from $tdir where the sanitized copies live. Scripts resolve the
    # binary via TEST_TMUX, never the cwd-relative ../tmux, so $tdir is safe.
    sed '/^PATH=\/bin:\/usr\/bin$/d' "$regress/$name" > "$tdir/$name"
    for inc in "$regress"/*.inc; do
        [ -e "$inc" ] || continue
        sed '/^PATH=\/bin:\/usr\/bin$/d' "$inc" > "$tdir/$(basename "$inc")"
    done
    # Scripts read fixtures (*.result, *.txt, conf, screen-redraw-results)
    # from their cwd; copy everything that is not a script alongside them.
    # --no-preserve=mode: the source may be a read-only nix store path.
    for f in "$regress"/*; do
        case $(basename "$f") in
        *.sh|*.inc|Makefile) continue ;;
        esac
        cp -r --no-preserve=mode "$f" "$tdir/"
    done
    log=/dev/null
    [ -n "${HAT_UPSTREAM_LOGS:-}" ] && log="$HAT_UPSTREAM_LOGS/$name.log"
    start=$(date +%s.%N)
    (
        cd "$tdir"
        TMUX_TMPDIR="$tdir" HOME="$tdir" TEST_TMUX="$hat_bin" HAT_PERSIST=0 \
            timeout 150 sh "$name"
    ) >"$log" 2>&1
    code=$?
    end=$(date +%s.%N)
    awk -v c="$code" -v s="$start" -v e="$end" -v n="$name" \
        'BEGIN { printf "%d %.1f %s\n", c, e - s, n }' >> "$results"
}
export -f run_one
[ -n "${HAT_UPSTREAM_LOGS:-}" ] && mkdir -p "$HAT_UPSTREAM_LOGS"
export regress hat_bin results HAT_UPSTREAM_LOGS

# Many scripts synchronise on fixed sleeps, so their outcome depends on how
# loaded the box is; the summary reports it to make a failure interpretable.
loadavg() { cut -d' ' -f1 /proc/loadavg 2>/dev/null || echo '?'; }
load_start=$(loadavg)
[ -n "$parallel_tests" ] &&
    printf '%s\n' $parallel_tests | xargs -P"$jobs" -I{} bash -c 'run_one "$@"' _ {}
for t in $serial_tests; do
    bash -c 'run_one "$@"' _ "$t"
done
load_end=$(loadavg)

pass=0; fail=0; xfail=0; xpass=0
failed_names=""; xpassed_names=""
outcomes="$work/outcomes"
: > "$outcomes"
while read -r code secs name; do
    expected_fail=false
    if [ -f "$xfail_file" ] && grep -q "^$name" "$xfail_file"; then
        expected_fail=true
    fi
    if [ "$code" -eq 0 ]; then
        if $expected_fail; then
            xpass=$((xpass + 1)); xpassed_names="$xpassed_names $name"
            outcome=XPASS
        else
            pass=$((pass + 1))
            outcome=PASS
        fi
    else
        if $expected_fail; then
            xfail=$((xfail + 1))
            outcome=XFAIL
        else
            fail=$((fail + 1)); failed_names="$failed_names $name"
            outcome=FAIL
        fi
    fi
    mode=""
    is_serial "$name" && mode=" (serial)"
    printf '%-5s %6ss  %s%s\n' "$outcome" "$secs" "$name" "$mode" >> "$outcomes"
done < "$results"

sort -k2 -rn "$outcomes"

echo "jobs=$jobs cores=$cores load(1m) start=$load_start end=$load_end"
echo "pass=$pass xfail=$xfail fail=$fail xpass=$xpass"
[ -n "$serial_tests" ] && echo "serial (run alone after the parallel batch):$serial_tests"
[ -n "$failed_names" ] && echo "FAILED:$failed_names"
[ -n "$xpassed_names" ] && echo "XPASSED (remove from xfail!):$xpassed_names"
[ "$fail" -eq 0 ] && [ "$xpass" -eq 0 ]
