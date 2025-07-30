import React from 'react';
import { useLocation, useNavigate } from 'react-router-dom';

interface LocationState {
  error?: string;
  reason?: string;
}

export const Error: React.FC = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const state = location.state as LocationState;
  
  const errorMessage = state?.error || state?.reason || 'An unknown error occurred';

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
      
      <h1>Transaction Failed</h1>
      
      <div style={{ 
        backgroundColor: '#f8d7da', 
        color: '#721c24',
        padding: '1rem', 
        borderRadius: '5px',
        marginBottom: '2rem',
        border: '1px solid #f5c6cb'
      }}>
        <h3>Error Details:</h3>
        <p>{errorMessage}</p>
      </div>

      <div style={{ marginBottom: '2rem' }}>
        <p>
          Your transaction could not be completed. This could be due to:
        </p>
        <ul>
          <li>Insufficient balance</li>
          <li>Network connectivity issues</li>
          <li>Invalid recipient address</li>
          <li>Blockchain network congestion</li>
        </ul>
      </div>

      <div style={{ marginBottom: '2rem' }}>
        <p>
          Please check your wallet balance and try again. 
          If the problem persists, please contact support.
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