// TODO: SubscriptionInterface with three parameters returned
const graphSubscription = (channel): any[] => {
  const event = (message) => {
    console.log('got graph event', message);
  };
  const err = (message) => {
    console.log('got graph error', message);
    channel.subscribe(...graphSubscription(channel));
  };
  const quit = (message) => {
    console.log('got graph quit', message);
    channel.subscribe(...graphSubscription(channel));
  };
  return [
    'graph-store',
    '/updates',
    { event, err, quit }
  ];
};

export default graphSubscription;