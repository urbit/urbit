import { pick } from 'lodash-es';
import React from 'react';
import { AppList } from '../../components/AppList';
import { Elbow } from '../../components/icons/Elbow';
import { useCharges } from '../../state/docket';
import { SystemNotification as SystemNotificationType } from '../../state/hark-types';

interface SystemNotificationProps {
  notification: SystemNotificationType;
}

export const SystemNotification = ({ notification }: SystemNotificationProps) => {
  const keys = notification.charges;
  const charges = useCharges();
  const blockedCharges = Object.values(pick(charges, keys));

  return (
    <section
      className="notification pl-12 text-black bg-orange-100"
      aria-labelledby="system-updates-blocked"
    >
      <header id="system-updates-blocked" className="relative -left-8 space-y-2">
        <div className="flex space-x-2">
          <span className="inline-block w-6 h-6 bg-orange-500 rounded-full" />
          <span className="font-medium">Landscape</span>
        </div>
        <div className="flex space-x-2">
          <Elbow className="w-6 h-6 text-gray-300" />
          <h2 id="blocked-apps">
            The following ({blockedCharges.length}) apps blocked a System Update:
          </h2>
          <AppList apps={blockedCharges} labelledBy="blocked-apps" />
        </div>
      </header>
    </section>
  );
};
