import Urbit from '@urbit/http-api';
import useHarkState from '~/logic/state/hark';
import useMetadataState from '~/logic/state/metadata';
import useContactState from '../state/contact';
import useGraphState from '../state/graph';
import useGroupState from '../state/group';
import useInviteState from '../state/invite';
import useLaunchState from '../state/launch';
import useSettingsState from '../state/settings';

const api = new Urbit('', '');
api.ship = window.ship;
api.verbose = true;

// @ts-ignore TODO window typings
window.api = api;

export const bootstrapApi = async () => {
  await api.poke({ app: 'hood', mark: 'helm-hi', json: 'opening airlock' });

  await api.eventSource();
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
    state.getState().initialize(api);
  });
};

export default api;
