import { App } from './App'
import { 
  AbstractGoogleWallet, 
} from 'zkfold-smart-wallet-api'

chrome.runtime.onMessage.addListener(async (request, _sender, _sendResponse) => {
    if (request.action === 'AUTH') {
        const app = await App.createFromEnv();
        app.initialiseGoogleWallet();
        if (app.wallet instanceof AbstractGoogleWallet) {
          const authUrl = app.wallet.createUrl();
          chrome.identity.launchWebAuthFlow(
              {
                  url: authUrl.toString(),
                  interactive: true
              },
              async (responseUrl?: string) => {
                if (app.wallet instanceof AbstractGoogleWallet) {
                  await app.wallet.oauthCallback(responseUrl || "")
                }
              }
          );
        }
    }
});

export { };
