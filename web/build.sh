#!/bin/sh
# Compila o WebAssembly para web/www/pkg. Com `serve`, também serve a página.
#
#   ./web/build.sh          só compila
#   ./web/build.sh serve    compila e serve em http://localhost:8080
set -e
cd "$(dirname "$0")/.."

wasm-pack build web --target web --out-dir www/pkg --release

if [ "$1" = "serve" ]; then
  echo "http://localhost:8080"
  exec python3 -m http.server 8080 -d web/www
fi
