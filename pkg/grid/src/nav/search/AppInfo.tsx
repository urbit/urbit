import React, { useEffect } from 'react';
import { useQuery } from 'react-query';
import { PillButton } from '../../components/Button';
import { DocketHeader } from '../../components/DocketHeader';
import { ShipName } from '../../components/ShipName';
import { Spinner } from '../../components/Spinner';
import { TreatyMeta } from '../../components/TreatyMeta';
import { useTreaty } from '../../logic/useTreaty';
import { chargesKey, fetchCharges } from '../../state/docket';
import { useLeapStore } from '../Nav';
import { useRecentsStore } from './Home';

export const AppInfo = () => {
  const addRecentApp = useRecentsStore((state) => state.addRecentApp);
  const select = useLeapStore((state) => state.select);
  const { ship, desk, treaty, installStatus, copyApp, installApp } = useTreaty();
  const { data: charges } = useQuery(chargesKey(), fetchCharges);
  const installed = (charges || {})[desk] || installStatus.isSuccess;

  useEffect(() => {
    select(
      <>
        Apps by <ShipName name={ship} className="font-mono" />: {treaty?.title}
      </>
    );
  }, [treaty?.title]);

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
              as="a"
              href={`/apps/${treaty.base}`}
              target={treaty.title || '_blank'}
              onClick={() => addRecentApp(treaty)}
            >
              Open App
            </PillButton>
          )}
          {!installed && (
            <PillButton onClick={installApp}>
              {installStatus.isIdle && 'Get App'}
              {installStatus.isLoading && (
                <>
                  <Spinner />
                  <span className="sr-only">Installing...</span>
                </>
              )}
            </PillButton>
          )}
          <PillButton variant="secondary" onClick={copyApp}>
            Copy App Link
          </PillButton>
        </div>
      </DocketHeader>
      <hr className="-mx-5 sm:-mx-8" />
      <TreatyMeta treaty={treaty} />
    </div>
  );
};
