#!/usr/bin/env bash
set -e

rm -rf dist
rm -rf node_modules

git pull
npm install
npm run build
npm run preview -- --port 8080