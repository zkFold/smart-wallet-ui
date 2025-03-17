import { createRequire } from "module";
const require = createRequire(import.meta.url);
const fs = require('fs').promises;
const path = require('path');
const process = require('process');
const nodemailer = require("nodemailer");
import * as dotenv from 'dotenv'
dotenv.config()

export async function sendMessage(recipient: string, subject: string, html: string) {
    const transporter = nodemailer.createTransport({
      host: "smtp.gmail.com",
      port: 465,
      secure: true,
      auth: {
        user: process.env.EMAIL_USER,
        pass: process.env.EMAIL_KEY
      }
    });
    const info = await transporter.sendMail({
        from: '"ZkFold Smart Wallet" <vmorozov@zkfold.io>', // sender address //TODO: update
        to: recipient, // list of receivers
        subject: subject, // Subject
        html: html 
      }).catch(console.error);

      return info ? info.messageId : null;
}

