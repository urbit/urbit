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
  airlock.reset();
  airlock.onError = (e) => {
    (async () => {
      const { reconnect, errorCount, set } = useLocalState.getState();
      console.log(errorCount);
      if(errorCount > 1) {
        set(s => {
          s.subscription = 'disconnected';
        });
        return;
      }
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

  useMetadataState.getState().initialize(airlock);

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

  const {
    getKeys,
    getShallowChildren
  } = useGraphState.getState();

  useHarkState.getState().getUnreads();
  getKeys();
  getShallowChildren(`~${window.ship}`, 'dm-inbox');
}
