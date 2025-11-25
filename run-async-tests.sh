#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "$(dirname "$0")"

echo "Installing npm dependencies..."
yarn install

echo "Compiling async tests..."
yarn shadow-cljs compile async-test

echo "Running async tests..."
node target/async-test.js