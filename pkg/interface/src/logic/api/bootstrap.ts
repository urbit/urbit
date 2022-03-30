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
import gcpManager from '../lib/gcpManager';

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

  await useMetadataState.getState().initialize(airlock);

  const path = window.location.pathname;
  const inGroup = path.startsWith('/apps/landscape/~landscape/ship');
  const inDms = path.startsWith('/apps/landscape/~landscape/messages');
  const {
    getKeys,
    getShallowChildren
  } = useGraphState.getState();

  if (inDms) {
    getShallowChildren(`~${window.ship}`, 'dm-inbox');
  }

  if (inGroup || inDms) {
    getKeys();
    useHarkState.getState().getUnreads();
  }

  const subs = [
    useGroupState,
    useContactState,
    useHarkState,
    useSettingsState,
    useInviteState,
    useStorageState,
    useLaunchState,
    useGraphState
  ].map(state => state.getState().initialize(airlock));

  await Promise.all(subs);

  useSettingsState.getState().getAll();
  gcpManager.start();
}
