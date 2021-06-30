import React from 'react';
import { Box } from '@tlon/indigo-react';
import WalletModal from './walletModal.js';
import ProviderModal from './providerModal.js';
import { useSettings } from '../../hooks/useSettings.js';

const StartupModal = () => {
  const { wallet, provider } = useSettings();
  let modal = null;

  if (wallet && provider) {
    return null;
  } else if (!provider) {
    modal = <ProviderModal />;
  } else if (!wallet) {
    modal = <WalletModal />;
  }
  return (
    <Box
      backgroundColor="scales.black20"
      left="0px"
      top="0px"
      width="100%"
      height="100%"
      position="fixed"
      display="flex"
      zIndex={10}
      justifyContent="center"
      alignItems="center"
    >
      <Box
        display="flex"
        flexDirection="column"
        width="400px"
        backgroundColor="white"
        borderRadius={3}
      >
        {modal}
      </Box>
    </Box>
  );
};

export default StartupModal;
