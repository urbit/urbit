import { chadIsRunning, Treaty } from '@urbit/api';
import clipboardCopy from 'clipboard-copy';
import React, { FC } from 'react';
import cn from 'classnames';
import { Vat } from '@urbit/api/hood';
import { Button, PillButton } from './Button';
import { Dialog, DialogClose, DialogContent, DialogTrigger } from './Dialog';
import { DocketHeader } from './DocketHeader';
import { Spinner } from './Spinner';
import { VatMeta } from './VatMeta';
import useDocketState, { ChargeWithDesk } from '../state/docket';
import { getAppHref } from '../state/util';
import { addRecentApp } from '../nav/search/Home';
import { TreatyMeta } from './TreatyMeta';

type InstallStatus = 'uninstalled' | 'installing' | 'installed';

type App = ChargeWithDesk | Treaty;
interface AppInfoProps {
  docket: App;
  vat?: Vat;
  className?: string;
}

function getInstallStatus(docket: App): InstallStatus {
  if (!('chad' in docket)) {
    return 'uninstalled';
  }
  if (chadIsRunning(docket.chad)) {
    return 'installed';
  }
  if ('install' in docket.chad) {
    return 'installing';
  }
  return 'uninstalled';
}

function getRemoteDesk(docket: App, vat?: Vat) {
  if (vat) {
    const { ship, desk } = vat!.arak;
    return [ship, desk];
  }
  if ('chad' in docket) {
    return ['', docket.desk];
  }
  const { ship, desk } = docket;
  return [ship, desk];
}

export const AppInfo: FC<AppInfoProps> = ({ docket, vat, className }) => {
  const installStatus = getInstallStatus(docket);
  const [ship, desk] = getRemoteDesk(docket, vat);

  const installApp = async () => {
    if (installStatus === 'installed') {
      return;
    }
    await useDocketState.getState().installDocket(ship, desk);
  };
  const copyApp = () => {
    clipboardCopy(`web+urbitgraph://app/${ship}/${desk}`);
  };

  if (!docket) {
    // TODO: maybe replace spinner with skeletons
    return (
      <div className="dialog-inner-container text-black">
        <span>Loading...</span>
      </div>
    );
  }

  return (
    <div className={cn('text-black', className)}>
      <DocketHeader docket={docket}>
        <div className="col-span-2 md:col-span-1 flex items-center space-x-4">
          {installStatus === 'installed' && (
            <PillButton
              variant="alt-primary"
              as="a"
              href={getAppHref(docket.href)}
              target={docket.desk || '_blank'}
              onClick={() => addRecentApp(docket)}
            >
              Open App
            </PillButton>
          )}
          {installStatus !== 'installed' && (
            <Dialog>
              <DialogTrigger as={PillButton} variant="alt-primary">
                {installStatus === 'installing' ? (
                  <>
                    <Spinner />
                    <span className="sr-only">Installing...</span>
                  </>
                ) : (
                  'Get App'
                )}
              </DialogTrigger>
              <DialogContent showClose={false} className="max-w-[400px] space-y-6">
                <h2 className="h4">Install &ldquo;{docket.title}&rdquo;</h2>
                <p className="text-base tracking-tight pr-6">
                  This application will be able to view and interact with the contents of your
                  Urbit. Only install if you trust the developer.
                </p>
                <div className="flex space-x-6">
                  <DialogClose as={Button} variant="secondary">
                    Cancel
                  </DialogClose>
                  <DialogClose as={Button} onClick={installApp}>
                    Get &ldquo;{docket.title}&rdquo;
                  </DialogClose>
                </div>
              </DialogContent>
            </Dialog>
          )}
          <PillButton variant="alt-secondary" onClick={copyApp}>
            Copy App Link
          </PillButton>
        </div>
      </DocketHeader>
      {vat ? (
        <>
          <hr className="-mx-5 sm:-mx-8" />
          <VatMeta vat={vat} />
        </>
      ) : null}
      {'chad' in docket ? null : (
        <>
          <hr className="-mx-5 sm:-mx-8" />
          <TreatyMeta treaty={docket} />
        </>
      )}
    </div>
  );
};
