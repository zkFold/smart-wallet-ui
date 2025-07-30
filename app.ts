import express from 'express';
import session from 'express-session';
import favicon from 'serve-favicon';
import * as crypto from 'crypto';
import * as http from 'http';
import bodyParser from 'body-parser';
import fs from 'fs-extra';
import unzip from 'unzip-stream';
import * as dotenv from 'dotenv';
import * as url from 'url';
import CSL from '@emurgo/cardano-serialization-lib-nodejs';

import { Wallet, WalletType, SmartTxRecipient } from 'zkfold-smart-wallet-api';
import { Backend, BigIntWrap } from 'zkfold-smart-wallet-api'
import { Notifier } from 'zkfold-smart-wallet-api'
import { GoogleApi } from 'zkfold-smart-wallet-api'

dotenv.config()

// Validate required environment variables
const requiredEnvVars = {
    EMAIL_USER: process.env.EMAIL_USER,
    EMAIL_KEY: process.env.EMAIL_KEY,
    CLIENT_ID: process.env.CLIENT_ID,
    CLIENT_SECRET: process.env.CLIENT_SECRET,
    WEBSITE_URL: process.env.WEBSITE_URL,
    BACKEND_URL: process.env.BACKEND_URL,
    SESSION_SECRET: process.env.SESSION_SECRET,
    BACKEND_API_KEY: process.env.BACKEND_API_KEY,
    PORT: process.env.PORT
};

// Parse the website URL to extract components
const websiteUrl = new URL(requiredEnvVars.WEBSITE_URL!);
const PORT = requiredEnvVars.PORT!;
const REDIRECT_URL = `${requiredEnvVars.WEBSITE_URL}/oauth2callback/`;

// Check for missing required environment variables
const missingVars = Object.entries(requiredEnvVars)
    .filter(([key, value]) => !value && key !== 'BACKEND_API_KEY')
    .map(([key]) => key);

if (missingVars.length > 0) {
    console.error(`Missing required environment variables: ${missingVars.join(', ')}`);
    process.exit(1);
}

const app = express();

const notifier = new Notifier(requiredEnvVars.EMAIL_USER!, requiredEnvVars.EMAIL_KEY!);
const gapi = new GoogleApi(requiredEnvVars.CLIENT_ID!, requiredEnvVars.CLIENT_SECRET!, REDIRECT_URL);

fs.createReadStream('./public/css.zip').pipe(unzip.Extract({ path: './public/' }));

app.use(express.static('public'));
app.use(bodyParser.urlencoded({ extended: true }));
app.use(favicon('./public/favicon.ico'));
app.use(session({
    secret: requiredEnvVars.SESSION_SECRET!,
    resave: false,
    saveUninitialized: false,
    cookie: { secure: websiteUrl.protocol === 'https:' } // Set to true for HTTPS proxy
}));

function loggedIn(req, res, next) {
    if (!req.session.initialiser) {
        res.redirect('/');
    } else {
        next();
    }
}

function restoreWallet(req) {
    const backendUrl = requiredEnvVars.BACKEND_URL!;
    const backend = requiredEnvVars.BACKEND_API_KEY
        ? new Backend(backendUrl, requiredEnvVars.BACKEND_API_KEY)
        : new Backend(backendUrl);
    const initialiser = req.session.initialiser;
    const wallet = new Wallet(backend, initialiser, '', req.session.network.toLowerCase());
    return wallet;
}

async function mkTransaction(req, res) {
    const wallet = restoreWallet(req);
    const balance = await wallet.getBalance();
    const address = await wallet.getAddress().then((x) => x.to_bech32());
    console.log(`Balance: ${balance}`);
    console.log(balance.lovelace);
    var assets = "";
    for (const [key, value] of Object.entries(balance)) {
        assets += `<li><b>${value}</b> <i>${key}</i></li>`
    }
    if (assets === "") {
        assets = "<li>No assets available</li>"
    }
    const template = fs.readFileSync('./transaction.html', 'utf-8');
    res.send(template.replaceAll('{ balance }', assets).replaceAll('{{ address }}', address));
}

app.get('/', async (req, res) => {
    res.sendFile('index.html', { root: '.' });
});

app.get('/wallet', loggedIn, mkTransaction);

app.get('/tx_status', async (req, res) => {
    let q = url.parse(req.url, true).query;
    if (!req.session.network) {
        res.send({ outcome: "failure", reason: 'Wallet not initialised' });
        return;
    }
    if (q.txId && q.recipient && typeof q.txId === 'string' && typeof q.recipient === 'string') {
        const txId = q.txId;
        const recipient: CSL.Address = CSL.Address.from_bech32(q.recipient);
        const backendUrl = requiredEnvVars.BACKEND_URL!;
        const backend = requiredEnvVars.BACKEND_API_KEY
            ? new Backend(backendUrl, requiredEnvVars.BACKEND_API_KEY)
            : new Backend(backendUrl);
        try {
            const utxos = await backend.addressUtxo(recipient); 
            for (var i = 0; i < utxos.length; i++) {
                const utxo = utxos[i];
                if ((utxo as any).ref.transaction_id == txId) { // Type assertion for now
                    res.send({ outcome: "success", "data": utxo });
                    return;
                }
            }
            res.send({ outcome: "pending" });
            return;
        } catch (e) {
            console.log(e);
            res.send({ outcome: "failure", reason: e });
            return;
        }

    }
    res.send({ outcome: "failure", reason: "no txId" });
});

app.post('/send', async (req, res) => {
    try {
        console.log(`Sending ${req.body.zkfold_amount} ${req.body.zkfold_asset} to ${req.body.zkfold_address} using ${req.body.recipient}`);
        var recipient;
        const asset_dict = {};
        asset_dict[`${req.body.zkfold_asset}`] = new BigIntWrap(req.body.zkfold_amount);
        switch (req.body.recipient) {
            case "Bech32": {
                recipient = new SmartTxRecipient(WalletType.Mnemonic, req.body.zkfold_address, asset_dict);
                break;
            };
            case "Gmail": {
                recipient = new SmartTxRecipient(WalletType.Google, req.body.zkfold_address, asset_dict);
                break;
            };
        }
        const wallet = restoreWallet(req);
        const txId = await wallet.sendTo(recipient);
        console.log(`tx id: ${txId}`);

        if (req.body.recipient == "Gmail") {
            const template = fs.readFileSync('./email.html', 'utf-8');
            const htmlText = template
                .replaceAll('{{ recipient }}', req.body.zkfold_address)
                .replaceAll('{{ website_url }}', requiredEnvVars.WEBSITE_URL!);
            await notifier.sendMessage(req.body.zkfold_address, "You've received funds", htmlText);
        }

        const template = fs.readFileSync('./success.html', 'utf-8');
        const addr = await wallet.addressForGmail(req.body.zkfold_address).then((x) => x.to_bech32());
        res.send(template.replaceAll('{ txId }', txId).replaceAll("{ recipient }", addr));
    } catch (error) {
        const template = fs.readFileSync('./failedTx.html', 'utf-8');
        res.send(template.replaceAll('{ reason }', `${error}`));
    }
});

app.post('/init', async (req, res) => {
    const state = crypto.randomBytes(32).toString('hex');
    // Store state in the session
    req.session.state = state;

    req.session.network = req.body.network;
    switch (req.body.method) {
        case "Mnemonic": {
            req.session.mnemonic = req.body.zkfold_method_data;
            res.redirect('/oauth2callback');
            break;
        };
        case "Google Oauth": {
            const authUrl = gapi.getAuthUrl(state);
            res.redirect(authUrl);
            break;
        };
    }
});

app.get('/oauth2callback', async (req, res) => {
    try {
        var initialiser;
        let q = url.parse(req.url, true).query;
        const backendUrl = requiredEnvVars.BACKEND_URL!;
        const backend = requiredEnvVars.BACKEND_API_KEY
            ? new Backend(backendUrl, requiredEnvVars.BACKEND_API_KEY)
            : new Backend(backendUrl);

        if (req.session.mnemonic) {
            initialiser = { method: WalletType.Mnemonic, data: req.session.mnemonic };
            req.session.mnemonic = null;
        } else if (q.error) { // An error response e.g. error=access_denied
            console.log('Error:' + q.error);
        } else if (q.state !== req.session.state) { //check state value
            console.log('State mismatch. Possible CSRF attack');
        } else if (q.code && typeof q.code === 'string') {
            const prvKey = CSL.Bip32PrivateKey
                .generate_ed25519_bip32()
                .derive(harden(1852)) // purpose
                .derive(harden(1815)) // coin type
                .derive(harden(0)) // account #0
                .derive(0)
                .derive(0);
            const jwt = await gapi.getJWT(q.code);
            console.log(jwt);
            console.log(`Root key: ${prvKey.to_raw_key().to_hex()}`);
            initialiser = { method: WalletType.Google, data: jwt, rootKey: prvKey.to_hex() };
        } else {
            console.log('Missing or invalid authorization code');
            res.redirect('/');
            return;
        }

        req.session.initialiser = initialiser;
        const wallet = new Wallet(backend, initialiser, '', req.session.network.toLowerCase());
        const balance = await wallet.getBalance();
        const addr = await wallet.getAddress().then((x) => x.to_bech32());
        console.log(balance);
        console.log(`Initialised a ${req.session.network} wallet with address ${addr}`);
        res.redirect('/wallet');
    } catch (e) {
        console.log(e);
        res.redirect('/')
    }
});


// Start HTTP server (HTTPS handled by proxy)
const httpServer = http.createServer(app);
httpServer.listen(PORT, () => {
    console.log(`HTTP server starting on port: ${PORT}`);
});

function harden(num: number): number {
    return 0x80000000 + num;
}
