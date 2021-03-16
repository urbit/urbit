import { SubscriptionRequestInterface, UrbitInterface } from "@urbit/http-api";
import useApi from "../api";
import useLocalState from "../state/local";

export const MAX_RESTART_COUNT = 3;

export const handleApiError = (channel: UrbitInterface) => {
  let tries = 0;
  return (error) => {
    console.error('API Connection Error', error);
    tries++;
    if (tries >= MAX_RESTART_COUNT) {
      useLocalState.setState({ connection: 'disconnected' });
    } else {
      // We can't really retry this
    }
  }
};

export const handleSubscriptionError = (
  channel: UrbitInterface,
  subscription: (channel: UrbitInterface) => SubscriptionRequestInterface
) => {
  let tries = 0;
  return (error) => {
    console.error('Susbcription Error', error, subscription);
    tries++;
    if (tries >= MAX_RESTART_COUNT) {
      useLocalState.setState({ connection: 'disconnected' });
    } else {
      const api = useApi();
      api.subscribe(subscription(channel));
    }
  }
}

export const handleSubscriptionQuit = (
  channel: UrbitInterface,
  subscription: (channel: UrbitInterface) => SubscriptionRequestInterface
) => {
  let tries = 0;
  return (quit) => {
    console.error('Subscription Quit', quit, subscription);
    tries++;
    if (tries >= MAX_RESTART_COUNT) {
      useLocalState.setState({ connection: 'disconnected' });
    } else {
      const api = useApi();
      api.subscribe(subscription(channel));
    }
  }
}

export const restartSubscription = async (existingId: number, subscription: SubscriptionRequestInterface) => {
  const api = useApi();
  await api.unsubscribe(existingId);
  api.subscribe(subscription);
  return;
};