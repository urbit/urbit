import { pick, partition } from 'lodash';
import React, { useCallback } from 'react';
import { kilnBump, Pike } from '@urbit/api';
import { useHistory } from 'react-router-dom';
import { AppList } from '../../components/AppList';
import { Button } from '../../components/Button';
import { Dialog, DialogClose, DialogContent, DialogTrigger } from '../../components/Dialog';
import { Elbow } from '../../components/icons/Elbow';
import api from '../../state/api';
import { useCharges } from '../../state/docket';
import useKilnState, { usePike } from '../../state/kiln';

import { NotificationButton } from './NotificationButton';
import { disableDefault } from '../../state/util';

export const RuntimeLagNotification = () => (
  <section
    className="notification pl-12 space-y-2 text-black bg-orange-50"
    aria-labelledby="runtime-lag"
  >
    <header id="system-updates-blocked" className="relative -left-8 space-y-2">
      <div className="flex space-x-2">
        <span className="inline-block w-6 h-6 bg-gray-200 rounded" />
        <span className="font-semibold">System</span>
      </div>
      <div className="flex space-x-2">
        <Elbow className="w-6 h-6 text-gray-300" />
        <h2 id="runtime-lag">The runtime blocked a System Update</h2>
      </div>
    </header>
    <div className="space-y-6">
      <p>
        In order to proceed with the System Update, you’ll need to upgrade the runtime. If you are
        using a hosted ship, you should contact your hosting provider.
      </p>
    </div>
  </section>
);

function pikeIsBlocked(newKelvin: number, pike: Pike) {
  return !pike.wefts?.find(({ kelvin }) => kelvin === newKelvin);
}

export const BaseBlockedNotification = () => {
  const basePike = usePike('base');
  const { push } = useHistory();
  // TODO: assert weft.name === 'zuse'??
  const newKelvin = basePike?.wefts[0]?.kelvin ?? 417;
  const charges = useCharges();
  const [blocked] = useKilnState((s) => {
    const [b, u] = partition(Object.entries(s.pikes), ([, pike]) => pikeIsBlocked(newKelvin, pike));
    return [b.map(([d]) => d), u.map(([d]) => d)] as const;
  });

  const blockedCharges = Object.values(pick(charges, blocked));
  const count = blockedCharges.length;

  const handlePauseOTAs = useCallback(() => {}, []);

  const handleArchiveApps = useCallback(async () => {
    api.poke(kilnBump());
    push('/leap/upgrading');
  }, []);

  return (
    <section
      className="notification pl-12 space-y-2 text-black bg-orange-50"
      aria-labelledby="system-updates-blocked"
    >
      <header id="system-updates-blocked" className="relative -left-8 space-y-2">
        <div className="flex space-x-2">
          <span className="inline-block w-6 h-6 bg-gray-200 rounded" />
          <span className="font-semibold">System</span>
        </div>
        <div className="flex space-x-2">
          <Elbow className="w-6 h-6 text-gray-300" />
          <h2 id="blocked-apps">The following ({count}) apps blocked a System Update:</h2>
        </div>
      </header>
      <AppList apps={blockedCharges} labelledBy="blocked-apps" size="xs" className="font-medium" />
      <div className="space-y-6">
        <p>
          In order to proceed with the System Update, you’ll need to temporarily archive these apps,
          which will render them unusable, but with data intact.
        </p>
        <p>
          Archived apps will automatically un-archive and resume operation when their developer
          provides an app update.
        </p>
      </div>
      <div className="space-x-2">
        <Dialog>
          <DialogTrigger as={NotificationButton}>Dismiss</DialogTrigger>
          <DialogContent
            showClose={false}
            className="space-y-6 text-base tracking-tight"
            containerClass="w-full max-w-md"
          >
            <h2 className="h4">Skip System Update</h2>
            <p>
              Skipping the application fo an incoming System Update will grant you the ability to
              continue using incompatible apps at the cost of an urbit that&apos;s not up to date.
            </p>
            <p>
              You can choose to apply System Updates from System Preferences any time.{' '}
              <a href="https://tlon.io" target="_blank" rel="noreferrer">
                Learn More
              </a>
            </p>
            <div className="flex space-x-6">
              <DialogClose as={Button} variant="secondary">
                Cancel
              </DialogClose>
              <DialogClose as={Button} variant="caution" onClick={handlePauseOTAs}>
                Pause OTAs
              </DialogClose>
            </div>
          </DialogContent>
        </Dialog>
        <Dialog>
          <DialogTrigger as={NotificationButton}>
            Archive ({count}) apps and Apply System Update
          </DialogTrigger>
          <DialogContent
            showClose={false}
            onOpenAutoFocus={disableDefault}
            className="space-y-6 text-base tracking-tight"
            containerClass="w-full max-w-md"
          >
            <h2 className="h4">Archive ({count}) Apps and Apply System Update</h2>
            <p>
              The following apps will be archived until their developer provides a compatible update
              to your system.
            </p>
            <AppList
              apps={blockedCharges}
              labelledBy="blocked-apps"
              size="xs"
              className="text-sm"
            />
            <div className="flex space-x-6">
              <DialogClose as={Button} variant="secondary">
                Cancel
              </DialogClose>
              <DialogClose as={Button} variant="caution" onClick={handleArchiveApps}>
                Archive Apps
              </DialogClose>
            </div>
          </DialogContent>
        </Dialog>
      </div>
    </section>
  );
};
