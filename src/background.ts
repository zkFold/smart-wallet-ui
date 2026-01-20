import { App } from './App'

chrome.runtime.onMessage.addListener(async (request, _sender, _sendResponse) => {
    if (request.action === 'AUTH') {
        const app = await App.createFromEnv()
        const authUrl = app.wallet.createUrl();
        chrome.identity.launchWebAuthFlow(
            {
                url: authUrl.toString(),
                interactive: true
            },
            async (responseUrl?: string) => {
                await app.wallet.oauthCallback(responseUrl || "")
            }
        );
    }
});

export { };