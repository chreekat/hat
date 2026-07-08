#!/usr/bin/env sh
# Run tmux's regress/ suite against hat.
#
# Usage: tools/run-upstream-tests.sh /path/to/tmux-source [test.sh ...]
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

tmux_src=${1:?usage: run-upstream-tests.sh /path/to/tmux-source [tests...]}
shift || true
regress="$tmux_src/regress"
[ -d "$regress" ] || { echo "no regress dir at $regress" >&2; exit 2; }

here=$(dirname "$0")
xfail_file="$here/upstream-xfail.txt"
hat_bin=$(cabal list-bin hat 2>/dev/null) || { echo "build hat first" >&2; exit 2; }

pass=0; fail=0; xfail=0; xpass=0
failed_names=""; xpassed_names=""

if [ $# -gt 0 ]; then
    tests="$*"
else
    tests=$(cd "$regress" && ls ./*.sh)
fi

for t in $tests; do
    name=$(basename "$t")
    ( cd "$regress" && TEST_TMUX="$hat_bin" timeout 30 sh "$name" ) \
        >/dev/null 2>&1
    code=$?
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
done

echo "pass=$pass xfail=$xfail fail=$fail xpass=$xpass"
[ -n "$failed_names" ] && echo "FAILED:$failed_names"
[ -n "$xpassed_names" ] && echo "XPASSED (remove from xfail!):$xpassed_names"
[ "$fail" -eq 0 ] && [ "$xpass" -eq 0 ]
