# Rex API revision 19 capture

One control connection and one terminal data stream, recorded against a real
Rex server: `rex 0.1.0-snapshot.2506` (commit 7635007b, API revision 19,
supports 0 to 19), on Linux.

## Files

| File | Content |
|---|---|
| `control.jsonl` | Every control message in order, one per line: `{"dir": "c2s" \| "s2c", "msg": ...}`. |
| `session_list.json` | Payload of the `session.list` response. |
| `session_view.json` | Payload of the `session.view` response. |
| `format.json` | Payload of `com.superlogical.terminal.format`: Rex's own text rendering of the final screen. |
| `stream.bin` | The data channel's binary messages after the WELCOME, concatenated, exactly as received. |
| `stream_meta.json` | The WELCOME's channel nonce and retention; `snapshot_len`, the offset where the snapshot ends and the raw terminal bytes start; `frames`, each binary message as `["bin", offset, length]`, then the close as `["close", code, reason]`. |
| `capture.py` | The script that wrote all of the above. |

## Request sequence

`hello` (`api_revision` 19, `api_min_revision` 19, client `cli`/`linux`/`hat`,
no `version`), then requests with ids counting from `"1"`:

1. `session.list`
2. `session.attach`
3. `data.connect` (focused block of the active window, `data_ref_id`
   `terminal`, subprotocol `rex.data.terminal.v1`)
4. `com.superlogical.terminal.write` of `printf '\033[31mred\033[0m hello-from-hat\n'` and a newline
5. `com.superlogical.terminal.format`
6. `session.view`
7. `session.destroy`

Requests 1 to 3 are the ones a client attaching to an existing session makes;
4 to 7 exist only to produce output, an oracle and a close. The data
connection is opened after response 3 and read for 1.5 s before request 4, for
1.5 s after it, and to its close (4000, `block closed`) after request 7.

## How it was captured

The server and the session's shell ran in an empty environment, with private
directories and a short runtime directory (a Unix socket path is limited to
108 bytes). `$D` is a fresh private directory, `$H` a private home directory inside it, and `$RT` a short path:

    E="env -i HOME=$H XDG_RUNTIME_DIR=$D/run XDG_CONFIG_HOME=$H/.config
       XDG_DATA_HOME=$H/.local/share XDG_STATE_HOME=$H/.local/state
       XDG_CACHE_HOME=$H/.cache REX_RUNTIME_DIR=$RT HISTFILE=/dev/null
       PATH=/usr/bin:/bin PS1='$ ' LANG=C TERM=xterm-256color"

    cd /
    $E rex server run --restore=false --state-dir $D/state --log-level debug &
    $E rex --autostart=false new main -c / --shell none --window main -- /bin/sh
    sleep 1
    python3 capture.py $RT/server.sock <outdir> main
    $E rex --autostart=false server stop

The session is created by the `rex` CLI on its own connection, before the
recorded one. `/bin/sh` was bash 5.3 in POSIX mode, started directly
(`--shell none`), so it read no startup files; the prompt is `$ ` and nothing
sets a title. The terminal is 80x24.

## What was scrubbed

`stream.bin` is byte-for-byte what the server sent. `stream_meta.json` is
unmodified.

In the JSON files `capture.py` replaces these values as it writes
(`CAPTURE_RAW=1` turns the replacement off, secrets excepted):

| Key | Written as | Why |
|---|---|---|
| `reconnect_token`, `ticket` | `"<redacted>"` | secrets |
| `pid` | `1001`, `1002`, ... in order of first appearance | host detail |
| `uid`, `gid`, `user_id` | `1000` | host detail (the real value was also 1000) |
| `display` | `"uid 1000"` | host detail (unchanged in practice) |
| `user` | `"user"` | account name |
| `path` (foreground process) | `"/usr/bin/bash"` | the real path named a package store directory |
| `connected_at` | `"2026-01-01T12:00:00.000000000+01:00"` | wall clock and time zone; the real value had the same layout |

Everything else is as received, including the ids (`session:`, `window:`,
`block:`, `layer:`, `client:`, `host_id`), which are random or time-based
UUIDs from a server state directory made for this capture.

The data channel's 32-byte WELCOME is not stored, because 16 of its bytes are
a reconnect token. Its other fields are in `stream_meta.json`.

## Consistency check

Decoding `stream.bin[0:1159]` with libghostty-vt's snapshot decoder and
writing `stream.bin[1159:]` into the resulting terminal gives an 80x24 screen
whose untrimmed text equals `format.json`'s `content`, trailing space of the
last prompt included. A 1158-byte prefix does not decode.
