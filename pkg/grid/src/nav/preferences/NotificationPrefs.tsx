import React from 'react';
import { Setting } from '../../components/Setting';
import { useSettingsState, SettingsState } from '../../state/settings';
import { usePreferencesStore } from './usePreferencesStore';

const selDnd = (s: SettingsState) => s.display.doNotDisturb;
async function toggleDnd() {
  const state = useSettingsState.getState();
  const curr = selDnd(state);
  if(curr) {
    Notification.requestPermission();
  }
  await state.putEntry('display', 'doNotDisturb', !curr);
}

export const NotificationPrefs = () => {
  const { mentions, toggleMentions } = usePreferencesStore();
  const doNotDisturb = useSettingsState(selDnd);

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
