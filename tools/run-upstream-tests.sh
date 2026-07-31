#!/usr/bin/env bash
# Run tmux's regress/ suite against hat, in parallel.
#
# Usage: tools/run-upstream-tests.sh /path/to/tmux-source [-j N] [test.sh ...]
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
# shell) so no path need be passed; an explicit first arg still overrides it.
if [ $# -gt 0 ] && [ "$1" != "-j" ]; then
    tmux_src=$1
    shift
else
    tmux_src=${HAT_TMUX_SRC:?usage: run-upstream-tests.sh /path/to/tmux-source [-j N] [tests...] (or enter the dev shell for $HAT_TMUX_SRC)}
fi

jobs=$(nproc 2>/dev/null || echo 4)
if [ "${1:-}" = "-j" ]; then
    jobs=$2
    shift 2
fi

regress="$tmux_src/regress"
[ -d "$regress" ] || { echo "no regress dir at $regress" >&2; exit 2; }

here=$(dirname "$0")
xfail_file="$here/upstream-xfail.txt"
hat_bin=$(cabal list-bin hat 2>/dev/null) || { echo "build hat first" >&2; exit 2; }

if [ $# -gt 0 ]; then
    tests=$(printf '%s\n' "$@" | xargs -n1 basename)
else
    tests=$(cd "$regress" && ls *.sh)
fi

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
    trap 'rm -rf "$tdir"' EXIT
    sed '/^PATH=\/bin:\/usr\/bin$/d' "$regress/$name" > "$tdir/$name"
    (
        cd "$regress"
        TMUX_TMPDIR="$tdir" HOME="$tdir" TEST_TMUX="$hat_bin" HAT_PERSIST=0 \
            timeout 30 sh "$tdir/$name"
    ) >/dev/null 2>&1
    printf '%d %s\n' "$?" "$name" >> "$results"
}
export -f run_one
export regress hat_bin results

printf '%s\n' $tests | xargs -P"$jobs" -I{} bash -c 'run_one "$@"' _ {}

pass=0; fail=0; xfail=0; xpass=0
failed_names=""; xpassed_names=""
while read -r code name; do
    expected_fail=false
    if [ -f "$xfail_file" ] && grep -q "^$name" "$xfail_file"; then
        expected_fail=true
    fi
    if [ "$code" -eq 0 ]; then
        if $expected_fail; then
            xpass=$((xpass + 1)); xpassed_names="$xpassed_names $name"
        else
            pass=$((pass + 1))
        fi
    else
        if $expected_fail; then
            xfail=$((xfail + 1))
        else
            fail=$((fail + 1)); failed_names="$failed_names $name"
        fi
    fi
done < "$results"

echo "pass=$pass xfail=$xfail fail=$fail xpass=$xpass"
[ -n "$failed_names" ] && echo "FAILED:$failed_names"
[ -n "$xpassed_names" ] && echo "XPASSED (remove from xfail!):$xpassed_names"
[ "$fail" -eq 0 ] && [ "$xpass" -eq 0 ]
