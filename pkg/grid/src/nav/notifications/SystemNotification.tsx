import { pick, partition } from 'lodash';
import React, { useCallback } from 'react';
import { HarkBin, HarkLid, kilnBump, Pike } from '@urbit/api';
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
import { useHarkStore } from '../../state/hark';

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
  return pike.zest === 'live' && !pike.wefts?.find(({ kelvin }) => kelvin === newKelvin);
}

export const BaseBlockedNotification = ({ bin, lid }: { bin: HarkBin, lid: HarkLid }) => {
  const basePike = usePike('base');
  const { push } = useHistory();
  // TODO: assert weft.name === 'zuse'??
  const newKelvin = basePike?.wefts[0]?.kelvin ?? 417;
  const charges = useCharges();
  const [blocked] = useKilnState((s) => {
    const [b, u] = partition(Object.entries(s.pikes), ([, pike]) => pikeIsBlocked(newKelvin, pike));
    return [b.map(([d]) => d), u.map(([d]) => d)] as const;
  });
  const { toggleInstall } = useKilnState();

  const blockedCharges = Object.values(pick(charges, blocked));
  const count = blockedCharges.length;

  const handlePauseOTAs = useCallback(async () => {
    await useHarkStore.getState().archiveNote(bin, lid);
  }, []);

  const handleArchiveApps = useCallback(async () => {
    await api.poke(kilnBump());
    await useHarkStore.getState().archiveNote(bin, lid);

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
          In order to proceed with the System Update, you’ll need to temporarily suspend these apps.
          This will render them unusable, but with data intact.
        </p>
        <p>
          Suspended apps will automatically resume operation when their developer
          provides an update.
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
            <h2 className="h4">Delay System Update</h2>
            <p>
              Are you sure you want to remain on an old version of Urbit
              until these apps have been updated?
            </p>
            <div className="flex space-x-6">
              <DialogClose as={Button} variant="secondary">
                Cancel
              </DialogClose>
              <DialogClose as={Button} variant="caution" onClick={handlePauseOTAs}>
                Remain on Old Version
              </DialogClose>
            </div>
          </DialogContent>
        </Dialog>
        <Dialog>
          <DialogTrigger as={NotificationButton}>
            Suspend ({count}) Apps and Apply Update
          </DialogTrigger>
          <DialogContent
            showClose={false}
            onOpenAutoFocus={disableDefault}
            className="space-y-6 text-base tracking-tight"
            containerClass="w-full max-w-md"
          >
            <h2 className="h4">Suspend ({count}) Apps and Apply System Update</h2>
            <p>
              The following apps will be suspended until their developer provides an update.
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
                Suspend Apps and Upgrade
              </DialogClose>
            </div>
          </DialogContent>
        </Dialog>
      </div>
    </section>
  );
};
