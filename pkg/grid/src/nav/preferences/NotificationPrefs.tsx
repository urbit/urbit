import React, { useEffect } from 'react';
import { Setting } from '../../components/Setting';
import { useLeapStore } from '../Nav';
import { usePreferencesStore } from './usePreferencesStore';

export const NotificationPrefs = () => {
  const select = useLeapStore((s) => s.select);
  const { doNotDisturb, mentions, toggleDoNotDisturb, toggleMentions } = usePreferencesStore();

  useEffect(() => {
    select('System Preferences: Notifications');
  }, []);

  return (
    <>
      <h2 className="h3 mb-7">Notifications</h2>
      <div className="space-y-3">
        <Setting on={doNotDisturb} toggle={toggleDoNotDisturb} name="Do Not Disturb">
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
