import React, { useEffect } from 'react';
import { useParams } from 'react-router-dom';
import { AppInfo } from '../../components/AppInfo';
import { ShipName } from '../../components/ShipName';
import { useCharge, useTreaty } from '../../state/docket';
import { useVat } from '../../state/kiln';
import { useLeapStore } from '../Nav';

export const TreatyInfo = () => {
  const select = useLeapStore((state) => state.select);
  const { ship, host, desk } = useParams<{ ship: string; host: string; desk: string }>();
  const treaty = useTreaty(host, desk);
  const vat = useVat(desk);
  const charge = useCharge(desk);

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
  return <AppInfo className="dialog-inner-container" docket={vat ? charge : treaty} vat={vat} />;

  /*
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
              onClick={() => addRecentApp(treaty)}
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
   */
};
