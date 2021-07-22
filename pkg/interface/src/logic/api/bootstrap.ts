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
import useStorageState from '../state/storage';

export async function bootstrapApi() {
  airlock.onError = (e) => {
    (async () => {
      const { reconnect } = useLocalState.getState();
      try {
        await reconnect();
      } catch (e) {
        console.log(e);
        console.log('onError');
      }
    })();
  };

  airlock.onRetry = () => {
    useLocalState.setState({ subscription: 'reconnecting' });
  };

  airlock.onOpen = () => {
    useLocalState.setState({ subscription: 'connected' });
  };

  [useGraphState].map(s => s.getState()?.clear?.());
  useGraphState.getState().getShallowChildren(`~${window.ship}`, 'dm-inbox');

  const promises = [
    useHarkState,
    useMetadataState,
    useGroupState,
    useContactState,
    useSettingsState,
    useLaunchState,
    useInviteState,
    useGraphState,
    useStorageState
  ].map(state => state.getState().initialize(airlock));
  await Promise.all(promises);
}

