import React from "react";
import { Text, LoadingSpinner, Button } from "@tlon/indigo-react";
import useApi from "~/logic/api";
import useLocalState from "~/logic/state/local";
import { restartSubscription } from "~/logic/lib/subscriptionHandlers";

const ReconnectButton = () => {
  const api = useApi();
  const connectedStatus = useLocalState(state => state.connection);
  const set = useLocalState(state => state.set);
  const reconnect = async () => {
    set(state => {
      state.connection = 'reconnecting';
    });
    await Promise.all(Array.from(api.outstandingSubscriptions.entries()).map(([existingId, subscription]) => {
      return restartSubscription(existingId, subscription);
    }));
    set(state => {
      state.connection = 'connected';
    });
  }

  if (connectedStatus === "disconnected") {
    return (
      <Button onClick={reconnect} borderColor='red' px='2'>
        <Text display={['none', 'inline']} textAlign='middle' color='red'>Reconnect</Text>
        <Text color='red'> ↻</Text>
      </Button>
    );
  } else if (connectedStatus === "reconnecting") {
    return (
      <Button borderColor='yellow' px='2' onClick={() => {}} cursor='default'>
        <LoadingSpinner pr={['0','2']} foreground='scales.yellow60' background='scales.yellow30'/>
        <Text display={['none', 'inline']} textAlign='middle' color='yellow'>Reconnecting</Text>
      </Button>
    )
  } else {
    return null;
  }
};

export default ReconnectButton;
