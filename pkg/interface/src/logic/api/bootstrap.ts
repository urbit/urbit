import airlock from '~/logic/api';
import useHarkState from '~/logic/state/hark';
import useMetadataState from '~/logic/state/metadata';
import useContactState from '../state/contact';
import useGraphState from '../state/graph';
import useGroupState from '../state/group';
import useInviteState from '../state/invite';
import useLaunchState from '../state/launch';
import useSettingsState from '../state/settings';
import useLocalState from '../state/local';

export const bootstrapApi = async () => {
  await airlock.poke({ app: 'hood', mark: 'helm-hi', json: 'opening airlock' });

  airlock.onError = (e) => {
    (async () => {
      try {
        useLocalState.setState({ subscription: 'reconnecting' });
        airlock.reset();
        await bootstrapApi();
      } catch (e) {
        useLocalState.setState({ subscription: 'disconnected' });
      }
    })();
  };

  airlock.onRetry = (e) => {
    useLocalState.setState({ subscription: 'reconnecting' });
  };

  airlock.onOpen = () => {
    useLocalState.setState({ subscription: 'connected' });
  };

  await airlock.eventSource();
  [
    useHarkState,
    useMetadataState,
    useGroupState,
    useContactState,
    useSettingsState,
    useLaunchState,
    useInviteState,
    useGraphState
  ].forEach((state) => {
    state.getState().initialize(airlock);
  });
};

