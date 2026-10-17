#!/usr/bin/env python3
"""Serve papers and talks while they are written: `make serve`.

Watches the sources, reruns make when one changes, and serves the
results over HTTP.  An HTML page reloads itself when it is rebuilt, and
shows the build log when the build fails.  A PDF is rebuilt and served,
but does not reload; use a viewer that reloads the file itself.

The reload script is added as a page is served, never to the file, so
what is built is exactly what is uploaded.

Only the Python standard library is used.
"""

import argparse
import html
import http.server
import json
import os
import re
import subprocess
import sys
import threading
import time
import urllib.parse
from pathlib import Path

# Directories whose files are never sources.
SKIP_DIRS = {".git", ".deps", "eln-cache", "__pycache__", "node_modules"}

# Org links to other files: what a paper transcludes or includes.
FILE_LINK = re.compile(r"\[\[file:([^]:\n]+)")

RELOAD_SCRIPT = """
<script>
/* Added by wg21-serve.py; not in the built file. */
(() => {
  const key = 'wg21-serve:' + location.pathname;
  try {
    const saved = JSON.parse(sessionStorage.getItem(key) || 'null');
    sessionStorage.removeItem(key);
    if (saved) {
      const restore = () => {
        if (window.Reveal && Reveal.isReady && Reveal.isReady()) {
          Reveal.slide(saved.h, saved.v, saved.f);
        } else if (window.Reveal && Reveal.on) {
          Reveal.on('ready', () => Reveal.slide(saved.h, saved.v, saved.f));
        } else {
          window.scrollTo(saved.x || 0, saved.y || 0);
        }
      };
      if (document.readyState === 'complete') restore();
      else window.addEventListener('load', restore);
    }
  } catch (e) {}
  const save = () => {
    try {
      const where = window.Reveal && Reveal.getIndices
        ? Reveal.getIndices() : {x: window.scrollX, y: window.scrollY};
      sessionStorage.setItem(key, JSON.stringify(where));
    } catch (e) {}
  };
  let banner = null;
  const showError = (log) => {
    if (!banner) {
      banner = document.createElement('div');
      banner.style.cssText = 'position:fixed;inset:0 0 auto 0;z-index:99999;' +
        'max-height:50vh;overflow:auto;margin:0;padding:.75em 1em;' +
        'background:#3d1e21;color:#ffd7d7;font:13px/1.4 ui-monospace,monospace;' +
        'white-space:pre-wrap;border-bottom:3px solid #ff6b6b';
      document.documentElement.appendChild(banner);
    }
    banner.textContent = 'Build failed; showing the last good version.\\n\\n' + log;
  };
  const events = new EventSource('/__wg21_serve/events?path=' +
                                 encodeURIComponent(location.pathname));
  events.addEventListener('reload', () => { save(); location.reload(); });
  events.addEventListener('failed', (e) => showError(JSON.parse(e.data).log));
  events.addEventListener('ok', () => { if (banner) { banner.remove(); banner = null; } });
})();
</script>
"""


def log(message):
    print(f"[serve {time.strftime('%H:%M:%S')}] {message}", flush=True)


class Builder:
    """Reruns make when the sources change, and records the outcome."""

    def __init__(self, root, make, targets, interval):
        self.root = root
        self.make = make
        self.targets = targets
        self.interval = interval
        self.changed = threading.Condition()
        self.generation = 0
        self.ok = True
        self.log = ""

    def sources(self):
        """Return the files that may go into a build.

        In a git checkout, the tracked and untracked files git does not
        ignore, so built files are left out; otherwise every file.  Plus
        any file an Org file links to, such as transcluded code.
        """
        try:
            listed = subprocess.run(
                ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"],
                cwd=self.root, capture_output=True, check=True).stdout
            files = {self.root / name for name in listed.decode().split("\0") if name}
        except (OSError, subprocess.CalledProcessError):
            files = set()
            for dirpath, dirnames, filenames in os.walk(self.root):
                dirnames[:] = [d for d in dirnames
                               if d not in SKIP_DIRS and not d.startswith("elpa")]
                files.update(Path(dirpath) / name for name in filenames)
        for org in [f for f in files if f.suffix == ".org"]:
            try:
                text = org.read_text(errors="replace")
            except OSError:
                continue
            for link in FILE_LINK.findall(text):
                files.add((org.parent / os.path.expanduser(link)).resolve())
        return files

    def snapshot(self):
        stamps = {}
        for path in self.sources():
            try:
                stamps[path] = path.stat().st_mtime_ns
            except OSError:
                pass
        return stamps

    def build(self):
        log(f"building: {' '.join(self.targets)}")
        started = time.monotonic()
        result = subprocess.run(
            [*self.make, "--no-print-directory", "-k", *self.targets],
            cwd=self.root, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        output = result.stdout.decode(errors="replace")
        lines = [line for line in output.splitlines()
                 if "obsolete macro" not in line
                 and not re.match(r"(Fontifing|Htmlizing) ", line)]
        elapsed = time.monotonic() - started
        if result.returncode == 0:
            log(f"built in {elapsed:.1f}s")
        else:
            print("\n".join(lines[-40:]), flush=True)
            log(f"build FAILED in {elapsed:.1f}s")
        with self.changed:
            self.ok = result.returncode == 0
            self.log = "\n".join(lines[-60:])
            self.generation += 1
            self.changed.notify_all()

    def run(self):
        stamps = self.snapshot()
        self.build()
        while True:
            time.sleep(self.interval)
            current = self.snapshot()
            if current != stamps:
                # Let an editor finish writing before building.
                time.sleep(0.2)
                stamps = self.snapshot()
                self.build()


class Handler(http.server.SimpleHTTPRequestHandler):
    builder = None
    targets = []

    def log_message(self, format, *args):
        pass

    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()

    def do_GET(self):
        url = urllib.parse.urlsplit(self.path)
        if url.path == "/__wg21_serve/events":
            return self.events(urllib.parse.parse_qs(url.query).get("path", ["/"])[0])
        if url.path == "/":
            return self.index()
        if url.path.endswith(".html"):
            return self.page(url.path)
        return super().do_GET()

    def outputs(self):
        root = Path(self.directory)
        return sorted(p.name for p in root.iterdir()
                      if p.suffix in (".html", ".pdf") and p.is_file())

    def index(self):
        named = [t for t in self.targets if t.endswith((".html", ".pdf"))]
        if len(named) == 1:
            self.send_response(302)
            self.send_header("Location", "/" + named[0])
            self.end_headers()
            return
        items = "".join(f'<li><a href="/{html.escape(name)}">{html.escape(name)}</a></li>\n'
                        for name in self.outputs())
        body = (f"<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">"
                f"<title>wg21org</title></head><body>\n<h1>wg21org</h1>\n"
                f"<ul>\n{items}</ul>\n{RELOAD_SCRIPT}</body></html>\n").encode()
        self.send(body, "text/html; charset=utf-8")

    def page(self, path):
        file = Path(self.translate_path(path))
        if not file.is_file():
            # Not built yet, or the build failed: wait for one that works.
            body = (f"<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">"
                    f"<title>{html.escape(path)}</title></head><body>\n"
                    f"<p>{html.escape(path)} has not been built.</p>\n"
                    f"{RELOAD_SCRIPT}</body></html>\n").encode()
            return self.send(body, "text/html; charset=utf-8", 404)
        text = file.read_bytes()
        at = text.rfind(b"</body>")
        if at < 0:
            at = len(text)
        self.send(text[:at] + RELOAD_SCRIPT.encode() + text[at:],
                  "text/html; charset=utf-8")

    def send(self, body, content_type, status=200):
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def events(self, path):
        """Stream reload and failure events for the page at PATH."""
        file = Path(self.translate_path(path))

        def stamp():
            try:
                return file.stat().st_mtime_ns
            except OSError:
                return None

        builder = self.builder
        name = file.name

        def failed(ok, build_log):
            # make -k names each target it could not make; a page is
            # told only of its own failure, or of one make cannot pin
            # on any target.
            if ok:
                return False
            failures = re.findall(r"\*\*\* \[[^]]*?([^]: ]+)\] Error", build_log)
            return not failures or name in failures

        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.end_headers()
        seen_stamp = stamp()
        with builder.changed:
            generation = builder.generation
            ok, build_log = builder.ok, builder.log
        try:
            if failed(ok, build_log):
                self.event("failed", {"log": build_log})
            while True:
                with builder.changed:
                    builder.changed.wait_for(lambda: builder.generation != generation, timeout=15)
                    if builder.generation == generation:
                        # A comment keeps the connection open.
                        self.wfile.write(b": keepalive\n\n")
                        self.wfile.flush()
                        continue
                    generation = builder.generation
                    ok, build_log = builder.ok, builder.log
                current = stamp()
                # A failed build may have deleted the page; keep showing
                # the last good one, with the log, until one succeeds.
                if current is not None and current != seen_stamp:
                    self.event("reload", {})
                    return
                if failed(ok, build_log):
                    self.event("failed", {"log": build_log})
                else:
                    self.event("ok", {})
        except (BrokenPipeError, ConnectionResetError):
            return

    def event(self, name, data):
        self.wfile.write(f"event: {name}\ndata: {json.dumps(data)}\n\n".encode())
        self.wfile.flush()


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8000)
    parser.add_argument("--make", default="make",
                        help="the make command, split on spaces")
    parser.add_argument("--interval", type=float, default=0.5,
                        help="seconds between checks for changed sources")
    parser.add_argument("targets", nargs="*", default=["all"])
    args = parser.parse_args()

    root = Path.cwd()
    builder = Builder(root, args.make.split(), args.targets or ["all"], args.interval)
    Handler.builder = builder
    Handler.targets = builder.targets

    server = http.server.ThreadingHTTPServer(
        (args.host, args.port),
        lambda *a, **kw: Handler(*a, directory=str(root), **kw))
    server.daemon_threads = True
    threading.Thread(target=builder.run, daemon=True).start()
    log(f"serving {root} at http://{args.host}:{args.port}/  (Ctrl-C to stop)")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
    return 0


if __name__ == "__main__":
    sys.exit(main())
