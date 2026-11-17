#!/usr/bin/env python3
"""Tiny OpenAI-compatible SSE server for the plain-runtime integration test."""
import json
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer

COUNT = 0


class Handler(BaseHTTPRequestHandler):
    def do_POST(self):
        global COUNT
        length = int(self.headers.get("content-length", "0"))
        self.rfile.read(length)
        COUNT += 1
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.end_headers()

        def event(obj):
            self.wfile.write(("data: " + json.dumps(obj) + "\n\n").encode())
            self.wfile.flush()

        if COUNT == 1:
            event({"choices": [{"delta": {"content": "hello"}}]})
            event({"choices": [{"delta": {}, "finish_reason": "stop"}]})
        elif COUNT == 2:
            event({"choices": [{"delta": {"tool_calls": [{
                "index": 0, "id": "call_1", "type": "function",
                "function": {"name": "set_result",
                             "arguments": json.dumps({"variable": "Out",
                                                      "value": "hello from mock"})}}]}}]})
            event({"choices": [{"delta": {}, "finish_reason": "tool_calls"}]})
        else:
            event({"choices": [{"delta": {"tool_calls": [{
                "index": 0, "id": "call_2", "type": "function",
                "function": {"name": "finish",
                             "arguments": json.dumps({"success": True})}}]}}]})
            event({"choices": [{"delta": {}, "finish_reason": "tool_calls"}]})
        self.wfile.write(b"data: [DONE]\n\n")
        self.wfile.flush()

    def log_message(self, *args):
        pass


if __name__ == "__main__":
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8769
    HTTPServer(("127.0.0.1", port), Handler).serve_forever()
