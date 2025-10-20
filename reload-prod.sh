#!/usr/bin/env bash
set -e

rm -rf dist
rm -rf node_modules

npm install
npm run build
npm run preview -- --port 8080