import React from 'react';
import { Icon, Row, Text } from '@tlon/indigo-react';
import { api } from '../../api';
import { useSettings } from '../../hooks/useSettings';

const CurrencyPicker = () => {
  const { denomination, currencyRates } = useSettings();
  const switchCurrency = () => {
    let newCurrency;
    if (denomination === 'BTC') {
      if (currencyRates['USD']) {
        newCurrency = 'USD';
      }
    } else if (denomination === 'USD') {
      newCurrency = 'BTC';
    }
    let setCurrency = {
      'put-entry': {
        value: newCurrency,
        'entry-key': 'currency',
        'bucket-key': 'btc-wallet',
      },
    };
    api.settingsEvent(setCurrency);
  };

  return (
    <Row style={{ cursor: 'pointer' }} onClick={() => switchCurrency()}>
      <Icon icon="ChevronDouble" color="orange" pt="2px" pr={1} />
      <Text color="orange" fontSize={1}>
        {denomination}
      </Text>
    </Row>
  );
};

export default CurrencyPicker;
