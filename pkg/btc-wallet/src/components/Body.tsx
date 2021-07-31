import React from 'react';
import { Box, LoadingSpinner, Col } from '@tlon/indigo-react';
import { Switch, Route } from 'react-router-dom';
import Balance from './Balance';
import Transactions from './Transactions/Transactions';
import Warning from './Warning';
import Header from './Header';
import Settings from './Settings';
import { useSettings } from '../hooks/useSettings';

const Body: React.FC = () => {
  const { loaded, showWarning: warning } = useSettings();
  const cardWidth = window.innerWidth <= 475 ? '350px' : '400px';
  return !loaded ? (
    <Box
      display="flex"
      width="100%"
      height="100%"
      alignItems="center"
      justifyContent="center"
    >
      <LoadingSpinner background="midOrange" foreground="orange" />
    </Box>
  ) : (
    <Switch>
      <Route path="/~btc/settings">
        <Col display="flex" flexDirection="column" width={cardWidth}>
          <Header settings={true} />
          <Settings />
        </Col>
      </Route>
      <Route path="/~btc">
        <Col display="flex" flexDirection="column" width={cardWidth}>
          <Header settings={false} />
          {!warning ? null : <Warning />}
          <Balance />
          <Transactions />
        </Col>
      </Route>
    </Switch>
  );
};

export default Body;
