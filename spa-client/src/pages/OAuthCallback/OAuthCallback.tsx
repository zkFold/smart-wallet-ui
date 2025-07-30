import React, { useEffect } from 'react';
import { useNavigate, useSearchParams } from 'react-router-dom';
import { useWalletStore } from '../../hooks/useWallet';

export const OAuthCallback: React.FC = () => {
  const navigate = useNavigate();
  const [searchParams] = useSearchParams();
  const { handleGoogleCallback, isLoading, error } = useWalletStore();

  useEffect(() => {
    const handleCallback = async () => {
      const code = searchParams.get('code');
      const state = searchParams.get('state');
      const errorParam = searchParams.get('error');

      if (errorParam) {
        console.error('OAuth error:', errorParam);
        navigate('/', { replace: true, state: { error: `OAuth error: ${errorParam}` } });
        return;
      }

      if (!code || !state) {
        console.error('Missing code or state parameter');
        navigate('/', { replace: true, state: { error: 'Missing authorization parameters' } });
        return;
      }

      try {
        await handleGoogleCallback(code, state);
        navigate('/wallet', { replace: true });
      } catch (error) {
        console.error('OAuth callback error:', error);
        navigate('/', { replace: true, state: { error: 'Failed to complete Google authentication' } });
      }
    };

    handleCallback();
  }, [searchParams, handleGoogleCallback, navigate]);

  if (error) {
    return (
      <main className="container">
        <h1>Authentication Error</h1>
        <p style={{ color: 'red' }}>{error}</p>
        <button onClick={() => navigate('/')}>Go Home</button>
      </main>
    );
  }

  return (
    <main className="container">
      <h1>Completing Google Authentication...</h1>
      {isLoading && <p>Please wait while we set up your wallet...</p>}
    </main>
  );
};