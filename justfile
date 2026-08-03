scrollback_leak_benchmark:
    ./tools/bench/scrollback_leak_benchmark
    @echo -e "\n>>> Open $(pwd)/hat-server.eventlog.html"

reload_rehydrate_profile blob='/tmp/hat-1000/default.reload.last':
    ./tools/bench/reload_rehydrate_profile {{blob}}
    @echo -e "\n>>> Open $(pwd)/reload-rehydrate.eventlog.html"
