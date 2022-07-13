import { Vat } from '@urbit/api';
import React from 'react';
import { AppList } from '../../components/AppList';
import { Button } from '../../components/Button';
import { Dialog, DialogClose, DialogContent, DialogTrigger } from '../../components/Dialog';
import { FullTlon16Icon } from '../../components/icons/FullTlon16Icon';
import { useSystemUpdate } from '../../logic/useSystemUpdate';
import { useCharge } from '../../state/docket';
import { useVat } from '../../state/kiln';
import { disableDefault, pluralize } from '../../state/util';
import { UpdatePreferences } from './UpdatePreferences';

function getHash(vat: Vat): string {
  const parts = vat.hash.split('.');
  return parts[parts.length - 1];
}

export const AboutSystem = () => {
  const garden = useVat('garden');
  const gardenCharge = useCharge('garden');
  const { base, update, systemBlocked, blockedCharges, blockedCount, freezeApps } =
    useSystemUpdate();
  const hash = base && getHash(base);
  const aeon = base ? base.arak.rail?.aeon : '';
  const nextAeon = update?.aeon;

  return (
    <>
      <div className="inner-section space-y-8 relative mb-4">
        <div className="flex items-center justify-between">
          <h2 className="h4">About System</h2>
          {systemBlocked && (
            <span className="bg-orange-50 text-orange-500 text-sm font-semibold px-2 py-1 rounded-md">
              System Update Blocked
            </span>
          )}
        </div>
        <div className="leading-5 space-y-4">
          <FullTlon16Icon className="h-4" />
          <div>
            <p>Landscape Operating Environment</p>
            <p>by Tlon Corporation</p>
          </div>
          <div>
            <p>
              Version {gardenCharge?.version} ({hash})
            </p>
            {systemBlocked && (
              <p>
                Aeon {aeon}{' '}
                <span className="text-orange-500 mx-4 space-x-2">
                  <span>&mdash;&gt;</span> <span>/</span> <span>&mdash;&gt;</span>
                </span>{' '}
                Aeon {nextAeon}
              </p>
            )}
          </div>
          {systemBlocked ? (
            <>
              <p className="text-orange-500">Update is currently blocked by the following apps:</p>
              <AppList
                apps={blockedCharges}
                labelledBy="blocked-apps"
                size="xs"
                className="font-medium"
              />
              <Dialog>
                <DialogTrigger as={Button} variant="caution">
                  Freeze {blockedCount} {pluralize('app', blockedCount)} and Apply Update
                </DialogTrigger>
                <DialogContent
                  showClose={false}
                  onOpenAutoFocus={disableDefault}
                  className="space-y-6 tracking-tight"
                  containerClass="w-full max-w-md"
                >
                  <h2 className="h4">
                    Freeze {blockedCount} {pluralize('App', blockedCount)} and Apply System Update
                  </h2>
                  <p>
                    The following apps will be archived until their developer provides a compatible
                    update to your system.
                  </p>
                  <AppList apps={blockedCharges} labelledBy="blocked-apps" size="xs" />
                  <div className="flex space-x-6">
                    <DialogClose as={Button} variant="secondary">
                      Cancel
                    </DialogClose>
                    <DialogClose as={Button} variant="caution" onClick={freezeApps}>
                      Freeze Apps
                    </DialogClose>
                  </div>
                </DialogContent>
              </Dialog>
            </>
          ) : (
            <p>Your urbit is up to date.</p>
          )}
        </div>
      </div>
      <UpdatePreferences base={base} />
    </>
  );
};
