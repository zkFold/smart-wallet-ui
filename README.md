# zkFold smart wallet backend, api and demo.

## Running the backend

Go to [atlas](atlas) and follow installation instructions. A sample preprod confog file is also there.
After installation, run 

```bash
cabal run zkfold-smart-wallet-server -- serve -c my_config.yaml
```

## Running the demo

Go to [e2e-test](e2e-test). Run

```bash
npm install
cd e2e
npx tsx e2e.ts
```

This should launch a server on [localhost](http://localhost:8080). Open the link in browser.
