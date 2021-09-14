import { setMentions } from '@urbit/api/dist';
import React from 'react';
import { Setting } from '../../components/Setting';
import { pokeOptimisticallyN } from '../../state/base';
import { HarkState, reduceGraph, useHarkStore } from '../../state/hark';
import { useSettingsState, SettingsState } from '../../state/settings';

const selDnd = (s: SettingsState) => s.display.doNotDisturb;
async function toggleDnd() {
  const state = useSettingsState.getState();
  await state.putEntry('display', 'doNotDisturb', !selDnd(state));
}

const selMentions = (s: HarkState) => s.notificationsGraphConfig.mentions;
async function toggleMentions() {
  const state = useHarkStore.getState();
  await pokeOptimisticallyN(useHarkStore, setMentions(!selMentions(state)), reduceGraph);
}

export const NotificationPrefs = () => {
  const doNotDisturb = useSettingsState(selDnd);
  const mentions = useHarkStore(selMentions);

  return (
    <>
      <h2 className="h3 mb-7">Notifications</h2>
      <div className="space-y-3">
        <Setting on={doNotDisturb} toggle={toggleDnd} name="Do Not Disturb">
          <p>
            Block visual desktop notifications whenever Urbit software produces an in-Landscape
            notification badge.
          </p>
          <p>
            Turning this &quot;off&quot; will prompt your browser to ask if you&apos;d like to
            enable notifications
          </p>
        </Setting>
        <Setting on={mentions} toggle={toggleMentions} name="Mentions">
          <p>
            [PLACEHOLDER] Block visual desktop notifications whenever Urbit software produces an
            in-Landscape notification badge.
          </p>
          <p>
            Turning this &quot;off&quot; will prompt your browser to ask if you&apos;d like to
            enable notifications
          </p>
        </Setting>
      </div>
    </>
  );
};
