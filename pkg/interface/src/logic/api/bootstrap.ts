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

export async function bootstrapApi() {
  airlock.onError = (e) => {
    (async () => {
      const { reconnect } = useLocalState.getState();
      try {
        useLocalState.setState({ subscription: 'reconnecting' });
        await reconnect();
      } catch (e) {
        useLocalState.setState({ subscription: 'disconnected' });
      }
    })();
  };

  airlock.onRetry = () => {
    useLocalState.setState({ subscription: 'reconnecting' });
  };

  airlock.onOpen = () => {
    useLocalState.setState({ subscription: 'connected' });
  };

  const promises = [
    useHarkState,
    useMetadataState,
    useGroupState,
    useContactState,
    useSettingsState,
    useLaunchState,
    useInviteState,
    useGraphState
  ].map(state => state.getState().initialize(airlock));
  await Promise.all(promises);
}

