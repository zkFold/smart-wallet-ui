import express from 'express';
import session from 'express-session';
import favicon from 'serve-favicon';
import axios from 'axios';
import * as crypto from 'crypto';
import * as https from 'https';
import * as http from 'http';
import bodyParser from 'body-parser';
import * as fs from 'fs';
import unzip from 'unzip-stream';
import fs from 'fs-extra';
import * as dotenv from 'dotenv';
import * as url from 'url';

import { Wallet, Initialiser, WalletType, SmartTxRecipient } from '../src/Wallet';
import { Backend } from '../src/Backend'
import { sendMessage } from '../src/GMail'
import { getJWT, getAuthUrl } from '../src/GoogleToken'

dotenv.config()

const app = express();

fs.createReadStream('./public/css.zip').pipe(unzip.Extract({ path: './public/' }));

app.use(express.static('public'));
app.use(bodyParser.urlencoded({ extended: true }));
app.use(favicon('./public/favicon.ico'));
app.use(session({
  secret: 'your_secure_secret_key', // Replace with a strong secret
  resave: false,
  saveUninitialized: false,
  cookie: { secure: process.env.PROTOCOL == "https" } // Set to true for HTTPS
}));

function loggedIn(req, res, next) {
  if(!req.session.initialiser) {
    res.redirect('/');
  } else {
    next();
  }
}

function restoreWallet(req) {
    const backend = new Backend('http://localhost:8082', '123');
    const initialiser = req.session.initialiser;
    const wallet = new Wallet(backend, initialiser, '', req.session.network.toLowerCase());
    return wallet;
}

async function mkTransaction(req, res) {
    const wallet = restoreWallet(req);
    const balance = await wallet.getBalance();
    const address = await wallet.getAddress().then((x) => x.to_bech32());
    console.log(balance);
    console.log(balance.lovelace);
    var ada = 0;
    if (Object.keys(balance).length > 0) {
        ada = Number(balance.lovelace);
    }
    const template = fs.readFileSync('./transaction.html', 'utf-8');
    res.send(template.replaceAll('{ balance }', ada / 1000000).replaceAll('{{ address }}', address));
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
    if (q.txId && q.recipient) {
        const txId = q.txId;
        const recipient = q.recipient;
        const backend = new Backend('http://localhost:8082', '123');
        try {
            const utxos = await backend.addressUtxo(recipient);
            for (var i = 0; i < utxos.length; i++) {
                const utxo = utxos[i];
                if (utxo.tx_hash == txId) {
                    res.send({ outcome: "success", "data": utxo });
                    return;
                }
            }
            res.send({ outcome: "pending" });
            return;
        } catch (e) {
            res.send({ outcome: "failure", reason: e });
            return;
        }

    }
    res.send({ outcome: "failure", reason: "no txId" });
});

app.post('/send', async (req, res) => {
//    try {
        console.log(`Sending ${req.body.zkfold_amount} ADA to ${req.body.zkfold_address} using ${req.body.recipient}`);
        var recipient;
        switch (req.body.recipient) {
            case "Bech32": {
                recipient = new SmartTxRecipient(WalletType.Mnemonic, req.body.zkfold_address, req.body.zkfold_amount * 1000000);
                break;
            };
            case "Gmail": {
                recipient = new SmartTxRecipient(WalletType.Google, req.body.zkfold_address, req.body.zkfold_amount * 1000000);
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
                                .replaceAll('{{ protocol }}', process.env.PROTOCOL)
                                .replaceAll('{{ host }}', process.env.HOST)
                                .replaceAll('{{ port }}', process.env.PORT);
                await sendMessage(req.body.zkfold_address, "You've received funds", htmlText);
        }

        const template = fs.readFileSync('./success.html', 'utf-8');
        const addr = await wallet.addressForGmail(req.body.zkfold_address).then((x) => x.to_bech32()); 
        res.send(template.replaceAll('{ txId }', txId).replaceAll("{ recipient }", addr));
//    } catch (error) {
//        const template = fs.readFileSync('./failedTx.html', 'utf-8');
//        res.send(template.replaceAll('{ reason }', `${error}`));
//    }
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
            const authUrl = getAuthUrl(state); 
            res.redirect(authUrl);
            break;
        };
    }
});

app.get('/oauth2callback', async (req, res) => {
    try {
        var initialiser;
        let q = url.parse(req.url, true).query;
        const backend = new Backend('http://localhost:8082', '123');

        if (req.session.mnemonic) {
            initialiser = { method: WalletType.Mnemonic, data: req.session.mnemonic };
            req.session.mnemonic = null;
        } else if (q.error) { // An error response e.g. error=access_denied
            console.log('Error:' + q.error);
        } else if (q.state !== req.session.state) { //check state value
            console.log('State mismatch. Possible CSRF attack');
        } else { 
            const jwt = await getJWT(q.code);
            initialiser = { method: WalletType.Google, data: jwt };
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


if (process.env.PROTOCOL == "https") {
    // We need to create these files in order to run a https server
    var key  = fs.readFileSync('./cert/selfsigned.key');
    var cert = fs.readFileSync('./cert/selfsigned.crt');
    var options = {
        key: key,
        cert: cert
    };

    var httpsServer = https.createServer(options, app);
    const port  = process.env.PORT;
    httpsServer.listen(port, () => {
        console.log("HTTPS server starting on port : " + port)
    });
};

if (process.env.PROTOCOL == "http") {
    var httpServer  = http.createServer(app);
    const port  = process.env.PORT;
    httpServer.listen(port, () => {
        console.log("HTTP server starting on port : " + port)
    });
}

