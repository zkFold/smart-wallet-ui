import { createRequire } from "module";
const require = createRequire(import.meta.url);

const http = require('http');
const url = require('url');
const open = require('open');
const destroyer = require('server-destroy');
const { google } = require('googleapis');
import * as dotenv from 'dotenv'
dotenv.config()

/**
 * To use OAuth2 authentication, we need access to a CLIENT_ID, CLIENT_SECRET, AND REDIRECT_URI
 * from the client_secret.json file. To get these credentials for your application, visit
 * https://console.cloud.google.com/apis/credentials.
 */
const oauth2Client = new google.auth.OAuth2(
  process.env.CLIENT_ID,
  process.env.CLIENT_SECRET,
  process.env.REDIRECT_URL
);

// Example access scopes for zkLogin: user email and public profile info are used.
const scopes = [
  'https://www.googleapis.com/auth/userinfo.email',
  'https://www.googleapis.com/auth/userinfo.profile',
  'openid',
];

export async function getJWT() {
    const authorizationUrl = oauth2Client.generateAuthUrl({
      // 'online' (default) or 'offline' (gets refresh_token)
      access_type: 'offline',
      /** Pass in the scopes array defined above.
        * Alternatively, if only one scope is needed, you can pass a scope URL as a string */
      scope: scopes,
      // Enable incremental authorization. Recommended as a best practice.
      include_granted_scopes: true,
    });

  console.log(`We are in getJWT`);
  return new Promise((resolve, reject) => {
    console.log(`Authorization URL: ${authorizationUrl}`);
    // Open an http server to accept the oauth callback. In this simple example, the
    // only request to our webserver is to /oauth2callback?code=<code>
    const server = http
      .createServer(async (req, res) => {
        try {
          if (req.url.indexOf('/oauth2callback') > -1) {
            // acquire the code from the querystring, and close the web server.
            const qs = new url.URL(req.url, 'http://localhost:3000')
              .searchParams;
            const code = qs.get('code');
            console.log(`Code is ${code}`);
            res.end('Authentication successful! Please return to the console.');
            server.destroy();

            // Now that we have the code, use that to acquire tokens.
            const { tokens } = await oauth2Client.getToken(code);
            console.info('Tokens acquired.');
            resolve(tokens);
          }
        } catch (e) {
          console.log('Error in getJWT', e);
          reject(e);
        }
      })
      .listen(3000, () => {
        console.log('Server listening on port 3000');
        // open the browser to the authorize url to start the workflow
        open(authorizationUrl, {wait: false})
        .then(cp => cp.unref())
        .catch(err => console.error('Failed to open URL:', err));
      });
    destroyer(server);
  });
}
