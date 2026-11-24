"""Capture one Rex control transcript and one terminal data stream (stdlib only).

Usage: capture.py <server-socket> <outdir> [session-label]

Attaches to an EXISTING session the way hat's milestone-1 client does (hello,
session.list, session.attach, data.connect), then makes output and records
Rex's own rendering of it. README.md has the server and session setup.
"""
import base64, json, os, select, socket, struct, sys, time

SOCK, OUT = sys.argv[1], sys.argv[2]
LABEL = sys.argv[3] if len(sys.argv) > 3 else None
RAW = os.environ.get("CAPTURE_RAW") == "1"  # skip the JSON scrub, for inspection only


class WS:
    def __init__(self, path, proto):
        self.s = socket.socket(socket.AF_UNIX)
        self.s.connect(SOCK)
        key = base64.b64encode(os.urandom(16)).decode()
        self.s.sendall(
            (f"GET {path} HTTP/1.1\r\nHost: rex\r\nUpgrade: websocket\r\n"
             f"Connection: Upgrade\r\nSec-WebSocket-Key: {key}\r\n"
             f"Sec-WebSocket-Version: 13\r\nSec-WebSocket-Protocol: {proto}\r\n\r\n").encode())
        self.buf = b""
        while b"\r\n\r\n" not in self.buf:
            self.buf += self.s.recv(65536)
        head, self.buf = self.buf.split(b"\r\n\r\n", 1)
        self.head = head.decode()
        assert " 101 " in self.head.split("\r\n")[0], self.head

    def send(self, op, data):
        mask = os.urandom(4)
        n = len(data)
        hdr = bytes([0x80 | op])
        if n < 126:
            hdr += bytes([0x80 | n])
        elif n < 65536:
            hdr += bytes([0x80 | 126]) + struct.pack(">H", n)
        else:
            hdr += bytes([0x80 | 127]) + struct.pack(">Q", n)
        self.s.sendall(hdr + mask + bytes(b ^ mask[i % 4] for i, b in enumerate(data)))

    def _need(self, n, deadline):
        while len(self.buf) < n:
            left = deadline - time.time()
            if left <= 0 or not select.select([self.s], [], [], left)[0]:
                return False
            chunk = self.s.recv(1 << 20)
            if not chunk:
                return False
            self.buf += chunk
        return True

    def recv(self, timeout):
        """One message as (opcode, payload), or None on timeout. Answers pings."""
        deadline = time.time() + timeout
        msg, mop = b"", None
        while True:
            if not self._need(2, deadline):
                return None
            b0, b1 = self.buf[0], self.buf[1]
            n, off = b1 & 0x7F, 2
            if n == 126:
                if not self._need(4, deadline):
                    return None
                n, off = struct.unpack(">H", self.buf[2:4])[0], 4
            elif n == 127:
                if not self._need(10, deadline):
                    return None
                n, off = struct.unpack(">Q", self.buf[2:10])[0], 10
            if not self._need(off + n, deadline):
                return None
            payload, self.buf = self.buf[off:off + n], self.buf[off + n:]
            op = b0 & 0x0F
            if op == 9:
                self.send(10, payload)
                continue
            if op == 10:
                continue
            if op == 8:
                return (8, payload)
            mop = op if op else mop
            msg += payload
            if b0 & 0x80:
                return (mop, msg)


log = open(os.path.join(OUT, "control.jsonl"), "w")

# Secrets: never written, scrubbed or not.
SECRET = ("reconnect_token", "ticket")
# Values that identify the capturing machine, replaced wherever the key occurs.
NEUTRAL = {
    "connected_at": "2026-01-01T12:00:00.000000000+01:00",
    "display": "uid 1000",
    "uid": 1000,
    "gid": 1000,
    "user_id": 1000,
    "user": "user",
    "path": "/usr/bin/bash",
}
pids = {}


def scrub(x):
    if isinstance(x, dict):
        out = {}
        for k, v in x.items():
            if k in SECRET:
                out[k] = "<redacted>"
            elif RAW:
                out[k] = scrub(v)
            elif k == "pid" and isinstance(v, int):
                out[k] = pids.setdefault(v, 1001 + len(pids))
            elif k in NEUTRAL and not isinstance(v, (dict, list)):
                out[k] = NEUTRAL[k]
            else:
                out[k] = scrub(v)
        return out
    if isinstance(x, list):
        return [scrub(v) for v in x]
    return x


def note(direction, obj):
    log.write(json.dumps({"dir": direction, "msg": scrub(obj)}) + "\n")
    log.flush()


def save(name, obj):
    with open(os.path.join(OUT, name), "w") as f:
        json.dump(scrub(obj), f, indent=1)
        f.write("\n")


ctl = WS("/api/control", "rex.control.v1")
rid = 0


def ctl_send(obj):
    note("c2s", obj)
    ctl.send(1, json.dumps(obj).encode())


def ctl_pump(timeout, want=None):
    end = time.time() + timeout
    while time.time() < end:
        m = ctl.recv(end - time.time())
        if m is None:
            return None
        if m[0] == 8:
            note("s2c-close", {"code": struct.unpack(">H", m[1][:2])[0], "reason": m[1][2:].decode()})
            return None
        obj = json.loads(m[1])
        note("s2c", obj)
        if want and obj.get("type") == "response" and obj.get("id") == want:
            return obj
        if want == "welcome" and obj.get("type") == "welcome":
            return obj
    return None


def call(method, payload=None):
    global rid
    rid += 1
    req = {"type": "request", "id": str(rid), "method": method}
    if payload is not None:
        req["payload"] = payload
    ctl_send(req)
    r = ctl_pump(10, str(rid))
    if r is None or "error" in r:
        print("FAILED", method, r)
        sys.exit(1)
    return r["payload"]


# --- The calls hat's client makes, in its order. ---
ctl_send({"type": "hello", "api_revision": 19, "api_min_revision": 19,
          "client": {"kind": "cli", "platform": "linux", "name": "hat"}})
welcome = ctl_pump(5, "welcome")
print("welcome:", {k: v for k, v in welcome.items() if k != "client"})

listing = call("session.list")
save("session_list.json", listing)
sessions = [s for s in listing["sessions"] if LABEL in (None, s["label"])]
sid = sessions[0]["session_id"]
attached = call("session.attach", {"session_id": sid})
view = attached["view"]
window = next(w for w in view["windows"] if w["window_id"] == view["active_window_id"])
bid = window["focused_block_id"]
tgt = {"session_id": sid, "block_id": bid}

ticket = call("data.connect", dict(tgt, data_ref_id="terminal", subprotocol="rex.data.terminal.v1"))
raw = base64.urlsafe_b64decode(ticket["ticket"] + "==")
data = WS("/api/data", "rex.data.terminal.v1")
data.send(2, b"\x01" + raw)
op, w = data.recv(8)
assert op == 2 and len(w) == 32 and w[0] == 0x81, (op, w)
retention, nonce = struct.unpack("<I", w[20:24])[0], struct.unpack("<Q", w[24:32])[0]
print("data WELCOME: retention_ms", retention, "nonce", nonce, "pad", w[17:20].hex())

stream = open(os.path.join(OUT, "stream.bin"), "wb")
frames = []


def drain(seconds):
    end = time.time() + seconds
    while time.time() < end:
        m = data.recv(end - time.time())
        if m is None:
            return
        if m[0] == 8:
            frames.append(("close", struct.unpack(">H", m[1][:2])[0], m[1][2:].decode()))
            return
        frames.append(("bin", stream.tell(), len(m[1])))
        stream.write(m[1])


drain(1.5)
mark1 = stream.tell()

# --- Capture-only extras: make output, then record Rex's own account of it. ---
call("com.superlogical.terminal.write", dict(tgt, args={"data": "printf '\\033[31mred\\033[0m hello-from-hat\\n'\n"}))
drain(1.5)
mark2 = stream.tell()
fmt = call("com.superlogical.terminal.format", tgt)
save("format.json", fmt)
save("session_view.json", call("session.view", {"session_id": sid}))
call("session.destroy", {"session_id": sid})
drain(2.0)
ctl_pump(0.5)
stream.close()

with open(os.path.join(OUT, "stream.bin"), "rb") as f:
    first = f.read(frames[0][2])
snapshot_len = first.find(b"\x1b_rex1;%d;" % nonce)
with open(os.path.join(OUT, "stream_meta.json"), "w") as f:
    json.dump({"nonce": nonce, "retention_ms": retention, "snapshot_len": snapshot_len,
               "after_snapshot_and_prompt": mark1, "after_printf": mark2, "frames": frames}, f, indent=1)
    f.write("\n")
print("stream bytes:", mark1, mark2, "snapshot_len:", snapshot_len, "frames:", frames)
