import { createRequire } from "module";
const require = createRequire(import.meta.url);

const http = require('http');
const https = require('https');
const url = require('url');
const express = require('express');
const session = require('express-session');
var passport = require('passport');
var FacebookStrategy = require('passport-facebook');


async function main() {
  const app = express();

  app.use(session({
    secret: 'your_secure_secret_key', // Replace with a strong secret
    resave: false,
    saveUninitialized: false,
  }));

  passport.use(new FacebookStrategy({
      clientID: '908839504716603',
      clientSecret: '02b058aa157be0f64e637cfbfbc87aba',
      callbackURL: "http://localhost:8080/authCallback",
      profileFields: ['id']
    },
    function(accessToken, refreshToken, profile, cb) {
      User.findOrCreate({ facebookId: profile.id }, function (err, user) {
        return cb(err, user);
      });
    }
  ));
  
  app.get('/auth', passport.authenticate('facebook'));

  const server = http.createServer(app);
  server.listen(8080);
}
main().catch(console.error);
