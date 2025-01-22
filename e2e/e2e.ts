import express from 'express';
import bodyParser from 'body-parser';
import { Wallet, Initialiser, Method, SmartTxRecipient, AddressType } from '../src/Wallet';
import { BlockFrostProvider } from '../src/Blockfrost';
import * as fs from 'fs';
import unzip from 'unzip-stream';
import fs from 'fs-extra';

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
    console.log(balance);
    console.log(balance.lovelace);
    const template = fs.readFileSync('./transaction.html', 'utf-8');
    res.send(template.replace('{ balance }', Number(balance.lovelace) / 1000000));
}

app.get('/', (req, res) => {
    res.sendFile('index.html', { root: '.' });
})

app.get('/wallet', loggedIn, mkTransaction); 

app.post('/send', async (req, res) => {
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

    const template = fs.readFileSync('./success.html', 'utf-8');
    res.send(template.replace('{ txId }', txId));
});

app.post('/init', async (req, res) => {
    var initialiser;
    switch (req.body.method) {
        case "Mnemonic": {
            initialiser = { method: Method.Mnemonic, data: req.body.method_data };
            break;
        };
        case "Google Oauth": {
            initialiser = { method: Method.Google, data: req.body.method_data };
            break;
        };
    }
    const provider = new BlockFrostProvider(req.body.network.toLowerCase())
    wallet = new Wallet(provider, req.body.wallet_name, initialiser, '', req.body.network.toLowerCase());
    const balance = await wallet.getBalance();
    console.log(balance);
    console.log(`Initialised a ${req.body.network} wallet ${req.body.wallet_name} with address ${wallet.getAddress().to_bech32()}`);
    res.redirect('/wallet');
});

app.listen(8080, () => {
    console.log('The application is listening on port 8080!');
})
