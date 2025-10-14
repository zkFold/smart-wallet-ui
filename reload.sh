#!/usr/bin/env bash
set -e

rm -rf dist
rm -rf node_modules
rm -f package-lock.json

npm install
npm run build
npm run dev