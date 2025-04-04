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

export function getAuthUrl(state) {
    const authorizationUrl = oauth2Client.generateAuthUrl({
      // 'online' (default) or 'offline' (gets refresh_token)
      access_type: 'offline',
      /** Pass in the scopes array defined above.
        * Alternatively, if only one scope is needed, you can pass a scope URL as a string */
      scope: scopes,
      // Enable incremental authorization. Recommended as a best practice.
      include_granted_scopes: true,
      // Include the state parameter to reduce the risk of CSRF attacks.
      state: state
    });
    return authorizationUrl;
};

export async function getJWT(code) {
    try {
        const { tokens } = await oauth2Client.getToken(code);
        console.info('Tokens acquired.');
        oauth2Client.setCredentials(tokens);
        return tokens.id_token;
    } catch (e) {
        console.log(e);
    }
}
