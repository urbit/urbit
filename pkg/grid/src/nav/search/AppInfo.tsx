import { chadIsRunning } from '@urbit/api/docket';
import clipboardCopy from 'clipboard-copy';
import React, { useEffect } from 'react';
import { useParams } from 'react-router-dom';
import { Button, PillButton } from '../../components/Button';
import { Dialog, DialogClose, DialogContent, DialogTrigger } from '../../components/Dialog';
import { DocketHeader } from '../../components/DocketHeader';
import { ShipName } from '../../components/ShipName';
import { Spinner } from '../../components/Spinner';
import { TreatyMeta } from '../../components/TreatyMeta';
import useDocketState, { useCharges, useTreaty } from '../../state/docket';
import { getAppHref } from '../../state/util';
import { useLeapStore } from '../Nav';

export const AppInfo = () => {
  const select = useLeapStore((state) => state.select);
  const { ship, host, desk } = useParams<{ ship: string; host: string; desk: string }>();
  const treaty = useTreaty(host, desk);
  const charges = useCharges();
  const charge = (charges || {})[desk];
  const installed = charge && chadIsRunning(charge.chad);
  const installing = charge && 'install' in charge.chad;

  useEffect(() => {
    select(
      <>
        Apps by <ShipName name={ship} className="font-mono" />: {treaty?.title}
      </>
    );
  }, [treaty?.title]);

  const installApp = async () => {
    await useDocketState.getState().installDocket(ship, desk);
  };
  const copyApp = () => {
    clipboardCopy(`web+urbitgraph://app/${ship}/${desk}`);
  };

  if (!treaty) {
    // TODO: maybe replace spinner with skeletons
    return (
      <div className="dialog-inner-container text-black">
        <span>Loading...</span>
      </div>
    );
  }

  return (
    <div className="dialog-inner-container text-black">
      <DocketHeader docket={treaty}>
        <div className="col-span-2 md:col-span-1 flex items-center space-x-4">
          {installed && (
            <PillButton
              variant="alt-primary"
              as="a"
              href={getAppHref(treaty.href)}
              target={treaty.title || '_blank'}
            >
              Open App
            </PillButton>
          )}
          {!installed && (
            <Dialog>
              <DialogTrigger as={PillButton} variant="alt-primary">
                {installing ? (
                  <>
                    <Spinner />
                    <span className="sr-only">Installing...</span>
                  </>
                ) : (
                  'Get App'
                )}
              </DialogTrigger>
              <DialogContent showClose={false} className="max-w-[400px] space-y-6">
                <h2 className="h4">Install &ldquo;{treaty.title}&rdquo;</h2>
                <p className="text-base tracking-tight pr-6">
                  This application will be able to view and interact with the contents of your
                  Urbit. Only install if you trust the developer.
                </p>
                <div className="flex space-x-6">
                  <DialogClose as={Button} variant="secondary">
                    Cancel
                  </DialogClose>
                  <DialogClose as={Button} onClick={installApp}>
                    Get &ldquo;{treaty.title}&rdquo;
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
      <hr className="-mx-5 sm:-mx-8" />
      <TreatyMeta treaty={treaty} />
    </div>
  );
};
