import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { useWalletStore } from '../../hooks/useWallet';
import { TransactionRequest } from '../../types';

export const Wallet: React.FC = () => {
  const navigate = useNavigate();
  const {
    isInitialized,
    balance,
    address,
    sendTransaction,
    refreshBalance,
    isLoading,
    error,
    setError
  } = useWalletStore();

  const [recipientType, setRecipientType] = useState<'Gmail' | 'Bech32'>('Gmail');
  const [recipient, setRecipient] = useState('');
  const [asset, setAsset] = useState('lovelace');
  const [amount, setAmount] = useState('');
  const [showAddress, setShowAddress] = useState(false);
  const [showAllControls, setShowAllControls] = useState(false);

  // Redirect to home if wallet is not initialized
  useEffect(() => {
    if (!isInitialized) {
      navigate('/');
    }
  }, [isInitialized, navigate]);

  // Refresh balance on component mount
  useEffect(() => {
    if (isInitialized) {
      refreshBalance().catch(console.error);
    }
  }, [isInitialized, refreshBalance]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError(null);

    if (!recipient.trim()) {
      setError('Please enter a recipient address');
      return;
    }

    if (!amount.trim() || isNaN(Number(amount)) || Number(amount) <= 0) {
      setError('Please enter a valid amount');
      return;
    }

    try {
      const request: TransactionRequest = {
        recipient: recipient.trim(),
        recipientType,
        asset: asset.trim() || 'lovelace',
        amount: amount.trim()
      };

      const txId = await sendTransaction(request);
      
      // Navigate to success page with transaction details
      navigate('/success', { 
        state: { 
          txId, 
          recipient: request.recipient,
          recipientType: request.recipientType,
          amount: request.amount,
          asset: request.asset
        } 
      });
    } catch (error) {
      console.error('Transaction error:', error);
      // Error is already set in the store
    }
  };

  const handleRecipientTypeChange = (newType: 'Gmail' | 'Bech32') => {
    setRecipientType(newType);
    setRecipient(''); // Clear recipient when changing type
  };

  const formatBalance = () => {
    if (!balance) return <li>No assets available</li>;
    
    return Object.entries(balance).map(([key, value]) => (
      <li key={key}>
        <b>{value}</b> <i>{key}</i>
      </li>
    ));
  };

  if (!isInitialized) {
    return null; // Will redirect via useEffect
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
      
      <h1>Perform a transaction</h1>
      
      <label>
        Wallet balance:
        <ul>
          {formatBalance()}
        </ul>
      </label>
      
      <form onSubmit={handleSubmit}>
        <fieldset>
          {showAllControls && (
            <label>
              Type of address
              <select 
                value={recipientType}
                onChange={(e) => handleRecipientTypeChange(e.target.value as 'Gmail' | 'Bech32')}
                required
              >
                <option value="Bech32">Bech32</option>
                <option value="Gmail">Gmail</option>
              </select>
            </label>
          )}
          
          <label>
            Recipient's address
            <input
              type="text"
              value={recipient}
              onChange={(e) => setRecipient(e.target.value)}
              placeholder={recipientType === 'Gmail' ? 'example@gmail.com' : 'addr_test1xyz...(Bech32)'}
              required
            />
          </label>
          
          {showAllControls && (
            <label>
              Asset name ('lovelace' for ADA or 'PolicyID.AssetName' for other assets)
              <input
                type="text"
                value={asset}
                onChange={(e) => setAsset(e.target.value)}
              />
            </label>
          )}
          
          <label>
            Amount
            <input
              type="number"
              step="any"
              min="0"
              value={amount}
              onChange={(e) => setAmount(e.target.value)}
              required
            />
          </label>
        </fieldset>
        
        {error && (
          <div style={{ color: 'red', marginBottom: '1rem' }}>
            {error}
          </div>
        )}
        
        <input
          type="submit"
          value="Send"
          disabled={isLoading}
        />
      </form>
      
      {showAddress && address && (
        <label style={{ marginTop: '1rem', display: 'block' }}>
          Use this address to receive funds from the{' '}
          <a href="https://docs.cardano.org/cardano-testnets/tools/faucet">Faucet</a>: {address}
        </label>
      )}
      
      <button 
        className="outline secondary" 
        onClick={() => setShowAddress(!showAddress)}
        type="button"
        style={{ marginTop: '1rem', marginRight: '0.5rem' }}
      >
        {showAddress ? 'Hide address' : 'Show address'}
      </button>
      
      <button 
        className="outline secondary" 
        onClick={() => {
          setShowAllControls(!showAllControls);
          if (showAllControls) {
            setRecipientType('Gmail');
            setAsset('lovelace');
          }
        }}
        type="button"
        style={{ marginTop: '1rem' }}
      >
        {showAllControls ? 'Hide address selector' : 'Show all controls'}
      </button>
      
      {isLoading && (
        <div style={{ marginTop: '1rem' }}>
          Processing transaction...
        </div>
      )}
    </main>
  );
};