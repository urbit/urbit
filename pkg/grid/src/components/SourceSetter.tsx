import React, { useCallback, useState } from 'react';
import { useAsyncCall } from '../logic/useAsyncCall';
import { Button } from './Button';
import { ShipName } from './ShipName';
import { Spinner } from './Spinner';

interface SourceSetterProps {
  appName: string;
  srcDesk: string;
  srcShip?: string;
  title: string;
  toggleSrc: (desk: string, ship: string) => Promise<void>;
}

export default function SourceSetter({
  appName,
  srcDesk,
  srcShip,
  title,
  toggleSrc
}: SourceSetterProps) {
  const [newSyncShip, setNewSyncShip] = useState(srcShip ?? '');
  const { status: requestStatus, call: handleSubmit } = useAsyncCall(toggleSrc);
  const syncDirty = newSyncShip !== srcShip;

  const onUnset = useCallback(() => {
    if (!srcShip) {
      return;
    }
    if (
      // eslint-disable-next-line no-alert, no-restricted-globals
      confirm(`Are you sure you want to unsync ${appName}? You will no longer receive updates.`)
    ) {
      toggleSrc(srcDesk, srcShip);
    }
  }, [srcShip, srcDesk]);

  const handleSourceChange = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    const { target } = e;
    const value = target.value.trim();
    setNewSyncShip(value.startsWith('~') ? value : `~${value}`);
  }, []);

  const onSubmit = useCallback(
    async (e: React.FormEvent<HTMLFormElement>) => {
      e.preventDefault();
      await handleSubmit(srcDesk, newSyncShip);
    },
    [srcDesk, newSyncShip]
  );

  return (
    <>
      <h2 className="h3 mb-7">{title}</h2>
      <div className="space-y-3">
        {srcShip ? (
          <>
            <h3 className="flex items-center h4 mb-2">Automatic Updates</h3>
            <p>Automatically download and apply updates to keep {appName} up to date.</p>
            <div className="flex-1 flex flex-col justify-center space-y-6">
              <p>
                OTA Source:{' '}
                <ShipName name={srcShip} truncate={false} className="font-semibold font-mono" />
              </p>
            </div>
            <div className="flex space-x-2">
              <Button onClick={onUnset} variant="destructive">
                Unsync Updates for {appName}...
              </Button>
            </div>
          </>
        ) : (
          <form className="inner-section relative" onSubmit={onSubmit}>
            <label htmlFor="ota-source" className="h4 mb-3">
              Set Update Source
            </label>
            <p className="mb-2">Enter a valid urbit name to receive updates for {appName}.</p>
            <div className="relative">
              <input
                id="ota-source"
                type="text"
                value={newSyncShip}
                onChange={handleSourceChange}
                className="input font-semibold default-ring"
              />
              {syncDirty && (
                <Button type="submit" className="absolute top-1 right-1 py-1 px-3 text-sm">
                  {requestStatus !== 'loading' && 'Save'}
                  {requestStatus === 'loading' && (
                    <>
                      <span className="sr-only">Saving...</span>
                      <Spinner className="w-5 h-5" />
                    </>
                  )}
                </Button>
              )}
            </div>
          </form>
        )}
      </div>
    </>
  );
}
