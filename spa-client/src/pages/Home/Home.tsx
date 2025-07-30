import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { useWalletStore } from '../../hooks/useWallet';

export const Home: React.FC = () => {
  const navigate = useNavigate();
  const { 
    initializeFromMnemonic, 
    startGoogleAuth, 
    isLoading, 
    error, 
    setError 
  } = useWalletStore();
  
  const [network, setNetwork] = useState<'Preview' | 'Preprod' | 'Mainnet'>('Preprod');
  const [method, setMethod] = useState<'Mnemonic' | 'Google Oauth'>('Google Oauth');
  const [mnemonic, setMnemonic] = useState('');
  const [showAdvanced, setShowAdvanced] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError(null);

    try {
      if (method === 'Mnemonic') {
        if (!mnemonic.trim()) {
          setError('Please enter a mnemonic phrase');
          return;
        }
        await initializeFromMnemonic(mnemonic, network);
        navigate('/wallet');
      } else {
        // Start Google OAuth flow
        const { authUrl } = startGoogleAuth();
        window.location.href = authUrl;
      }
    } catch (error) {
      console.error('Initialization error:', error);
      // Error is already set in the store
    }
  };

  const toggleAdvanced = () => {
    setShowAdvanced(!showAdvanced);
    if (!showAdvanced) {
      // Reset to default values when showing advanced
      setNetwork('Preprod');
      setMethod('Google Oauth');
    }
  };

  const handleMethodChange = (newMethod: 'Mnemonic' | 'Google Oauth') => {
    setMethod(newMethod);
    setMnemonic(''); // Clear mnemonic when switching methods
  };

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
      
      <h1>
        {showAdvanced ? 'Smart Wallet' : 'Smart Wallet (Preprod)'}
      </h1>
      
      <form onSubmit={handleSubmit}>
        <fieldset>
          {showAdvanced && (
            <label>
              Network type
              <select 
                value={network} 
                onChange={(e) => setNetwork(e.target.value as 'Preview' | 'Preprod' | 'Mainnet')}
                required
              >
                <option value="Preview">Preview</option>
                <option value="Preprod">Preprod</option>
                {/* <option value="Mainnet">Mainnet</option> */}
              </select>
            </label>
          )}
          
          {showAdvanced && (
            <label>
              Method
              <select 
                value={method}
                onChange={(e) => handleMethodChange(e.target.value as 'Mnemonic' | 'Google Oauth')}
                required
              >
                <option value="Mnemonic">Mnemonic</option>
                <option value="Google Oauth">Google Oauth</option>
              </select>
            </label>
          )}
          
          {method === 'Mnemonic' && (
            <input
              type="text"
              value={mnemonic}
              onChange={(e) => setMnemonic(e.target.value)}
              placeholder="Enter your mnemonic phrase"
              autoComplete="off"
              required
            />
          )}
        </fieldset>
        
        {error && (
          <div style={{ color: 'red', marginBottom: '1rem' }}>
            {error}
          </div>
        )}
        
        <input
          type="submit"
          value={
            method === 'Google Oauth' 
              ? 'Initialise wallet with Gmail' 
              : 'Initialise wallet with seedphrase'
          }
          disabled={isLoading}
        />
      </form>
      
      <button 
        className="outline secondary" 
        onClick={toggleAdvanced}
        type="button"
      >
        {showAdvanced ? 'Hide advanced controls' : 'Show advanced controls'}
      </button>
      
      {isLoading && (
        <div style={{ marginTop: '1rem' }}>
          Initializing wallet...
        </div>
      )}
    </main>
  );
};