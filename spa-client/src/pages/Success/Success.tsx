import React, { useEffect, useState } from 'react';
import { useLocation, useNavigate } from 'react-router-dom';
import { backendApi } from '../../services/backendApi';
import { walletService } from '../../services/walletService';

interface LocationState {
  txId: string;
  recipient: string;
  recipientType: 'Gmail' | 'Bech32';
  amount: string;
  asset: string;
}

export const Success: React.FC = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const state = location.state as LocationState;
  const [recipientAddress, setRecipientAddress] = useState<string>('');

  useEffect(() => {
    const getRecipientAddress = async () => {
      if (!state) return;
      
      if (state.recipientType === 'Gmail') {
        try {
          const address = await walletService.getAddressForGmail(state.recipient);
          setRecipientAddress(address);
        } catch (error) {
          console.error('Error getting Gmail address:', error);
          setRecipientAddress(state.recipient);
        }
      } else {
        setRecipientAddress(state.recipient);
      }
    };

    getRecipientAddress();
  }, [state]);

  if (!state) {
    return (
      <main className="container">
        <h1>No transaction data</h1>
        <button onClick={() => navigate('/')}>Go Home</button>
      </main>
    );
  }

  return (
    <main className="container">
      <a href="https://zkfold.io">
        <img 
          src="/logo-200x73.png" 
          alt="zkFold Logo" 
          style={{ width: '250px', height: '100px' }} 
        />
      </a>
      <br />
      <br />
      
      <h1>Transaction Sent Successfully!</h1>
      
      <div style={{ marginBottom: '2rem' }}>
        <h3>Transaction Details:</h3>
        <ul>
          <li><strong>Transaction ID:</strong> {state.txId}</li>
          <li><strong>Amount:</strong> {state.amount} {state.asset}</li>
          <li><strong>Recipient:</strong> {state.recipient}</li>
          {recipientAddress && recipientAddress !== state.recipient && (
            <li><strong>Recipient Address:</strong> {recipientAddress}</li>
          )}
        </ul>
      </div>

      {state.recipientType === 'Gmail' && (
        <div style={{ 
          backgroundColor: '#e8f5e8', 
          padding: '1rem', 
          borderRadius: '5px',
          marginBottom: '2rem'
        }}>
          <p>
            📧 An email notification has been sent to {state.recipient} 
            informing them about the received funds.
          </p>
        </div>
      )}

      <div style={{ marginBottom: '2rem' }}>
        <p>
          Your transaction has been submitted to the blockchain. 
          It may take a few minutes to confirm.
        </p>
        <p>
          You can track the transaction status using the transaction ID above.
        </p>
      </div>

      <div style={{ display: 'flex', gap: '1rem' }}>
        <button 
          onClick={() => navigate('/wallet')}
          className="primary"
        >
          Back to Wallet
        </button>
        <button 
          onClick={() => navigate('/')}
          className="outline"
        >
          Go Home
        </button>
      </div>
    </main>
  );
};