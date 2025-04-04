import express from 'express';
import * as https from 'https';
import * as http from 'http';
import bodyParser from 'body-parser';
import * as fs from 'fs';
import unzip from 'unzip-stream';
import fs from 'fs-extra';
import * as dotenv from 'dotenv'

import { Wallet, Initialiser, Method, SmartTxRecipient, AddressType } from '../src/Wallet';
import { BlockFrostProvider } from '../src/Blockfrost';
import { sendMessage } from '../src/GMail'
import { getJWT } from '../src/GoogleToken'

dotenv.config()

const app = express();

var wallet = null;

fs.createReadStream('./public/css.zip').pipe(unzip.Extract({ path: './public/' }));

app.use(express.static('public'));
app.use(bodyParser.urlencoded({ extended: true }));

function loggedIn(req, res, next) {
  if(wallet == null) {
    res.redirect('/');
  } else {
    next();
  }
}

async function mkTransaction(req, res) {
    const balance = await wallet.getBalance();
    const address = wallet.getAddress().to_bech32();
    console.log(balance);
    console.log(balance.lovelace);
    var ada = 0;
    if (Object.keys(balance).length > 0) {
        ada = Number(balance.lovelace);
    }
    const template = fs.readFileSync('./transaction.html', 'utf-8');
    res.send(template.replace('{ balance }', ada / 1000000).replace('{{ address }}', address));
}

app.get('/', async (req, res) => {
    res.sendFile('index.html', { root: '.' });
})

app.get('/wallet', loggedIn, mkTransaction); 

app.post('/send', async (req, res) => {
    try {
        console.log(`Sending ${req.body.amount} ADA to ${req.body.address} using ${req.body.recipient}`);
        var recipient;
        switch (req.body.recipient) {
            case "Bech32": {
                recipient = new SmartTxRecipient(AddressType.Bech32, req.body.address, req.body.amount);
                break;
            };
            case "Gmail": {
                recipient = new SmartTxRecipient(AddressType.Gmail, req.body.address, req.body.amount);
                break;
            };
        }
        const txId = await wallet.sendTo(recipient);

        if (req.body.recipient == "Gmail") {
                const template = fs.readFileSync('./email.html', 'utf-8');
                const htmlText = template
                                .replace('{{ recipient }}', req.body.address)
                                .replace('{{ protocol }}', process.env.PROTOCOL)
                                .replace('{{ host }}', process.env.HOST)
                                .replace('{{ port }}', process.env.PORT);
                await sendMessage(req.body.address, "You've received funds", htmlText);
        }

        const template = fs.readFileSync('./success.html', 'utf-8');
        res.send(template.replace('{ txId }', txId));
    } catch (error) {
        const template = fs.readFileSync('./failedTx.html', 'utf-8');
        res.send(template.replace('{ reason }', `${error}`));
    }
});

app.post('/init', async (req, res) => {
    var initialiser;
    switch (req.body.method) {
        case "Mnemonic": {
            initialiser = { method: Method.Mnemonic, data: req.body.method_data };
            break;
        };
        case "Google Oauth": {
            const token = await getJWT();
            const jwt = token.id_token;
            initialiser = { method: Method.Google, data: jwt };
            break;
        };
    }
    const provider = new BlockFrostProvider(req.body.network.toLowerCase())
    wallet = new Wallet(provider, initialiser, '', req.body.network.toLowerCase());
    const balance = await wallet.getBalance();
    console.log(balance);
    console.log(`Initialised a ${req.body.network} wallet with address ${wallet.getAddress().to_bech32()}`);
    res.redirect('/wallet');
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

