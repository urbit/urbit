import { chadIsRunning, Treaty, Vat } from '@urbit/api';
import clipboardCopy from 'clipboard-copy';
import React, { FC, useCallback, useState } from 'react';
import cn from 'classnames';
import { Button, PillButton } from './Button';
import { Dialog, DialogClose, DialogContent, DialogTrigger } from './Dialog';
import { DocketHeader } from './DocketHeader';
import { Spinner } from './Spinner';
import { VatMeta } from './VatMeta';
import useDocketState, { ChargeWithDesk, useTreaty } from '../state/docket';
import { getAppHref, getAppName } from '../state/util';
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
  if (vat && vat.arak.rail) {
    const { ship, desk } = vat.arak.rail;
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
  const publisher = vat?.arak?.rail?.publisher ?? ship;
  const [copied, setCopied] = useState(false);
  const treaty = useTreaty(ship, desk);

  const installApp = async () => {
    if (installStatus === 'installed') {
      return;
    }
    await useDocketState.getState().installDocket(ship, desk);
  };

  const copyApp = useCallback(() => {
    setCopied(true);
    clipboardCopy(`web+urbitgraph://${publisher}/${desk}`);

    setTimeout(() => {
      setCopied(false);
    }, 1250);
  }, [publisher, desk]);

  const installing = installStatus === 'installing';

  if (!docket) {
    // TODO: maybe replace spinner with skeletons
    return (
      <div className="dialog-inner-container flex justify-center text-black">
        <Spinner className="w-10 h-10" />
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
              target="_blank"
              rel="noreferrer"
              onClick={() => addRecentApp(docket.desk)}
            >
              Open App
            </PillButton>
          )}
          {installStatus !== 'installed' && (
            <Dialog>
              <DialogTrigger as={PillButton} disabled={installing} variant="alt-primary">
                {installing ? (
                  <>
                    <Spinner />
                    <span className="sr-only">Installing...</span>
                  </>
                ) : (
                  'Get App'
                )}
              </DialogTrigger>
              <DialogContent
                showClose={false}
                className="space-y-6"
                containerClass="w-full max-w-md"
              >
                <h2 className="h4">Install &ldquo;{getAppName(docket)}&rdquo;</h2>
                <p className="tracking-tight pr-6">
                  This application will be able to view and interact with the contents of your
                  Urbit. Only install if you trust the developer.
                </p>
                <div className="flex space-x-6">
                  <DialogClose as={Button} variant="secondary">
                    Cancel
                  </DialogClose>
                  <DialogClose as={Button} onClick={installApp}>
                    Get &ldquo;{getAppName(docket)}&rdquo;
                  </DialogClose>
                </div>
              </DialogContent>
            </Dialog>
          )}
          <PillButton variant="alt-secondary" onClick={copyApp}>
            {!copied && 'Copy App Link'}
            {copied && 'copied!'}
          </PillButton>
        </div>
      </DocketHeader>
      <div className="space-y-6">
        {vat ? (
          <>
            <hr className="-mx-5 sm:-mx-8 border-gray-50" />
            <VatMeta vat={vat} />
          </>
        ) : null}
        {!treaty ? null : (
          <>
            <hr className="-mx-5 sm:-mx-8 border-gray-50" />
            <TreatyMeta treaty={treaty} />
          </>
        )}
      </div>
    </div>
  );
};
