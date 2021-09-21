import _ from 'lodash';
import React, { ChangeEvent, FormEvent, useCallback, useEffect, useState } from 'react';
import { Button } from '../../components/Button';
import { Setting } from '../../components/Setting';
import { ShipName } from '../../components/ShipName';
import { Spinner } from '../../components/Spinner';
import { useAsyncCall } from '../../logic/useAsyncCall';
import useKilnState, { useVat } from '../../state/kiln';

export const SystemUpdatePrefs = () => {
  const { changeOTASource, toggleOTAs } = useKilnState((s) =>
    _.pick(s, ['toggleOTAs', 'changeOTASource'])
  );
  const base = useVat('base');
  const otasEnabled = base && !base.arak.paused;
  const otaSource = base?.arak.ship;

  const toggleBase = useCallback((on: boolean) => toggleOTAs('base', on), [toggleOTAs]);

  const [source, setSource] = useState('');
  const sourceDirty = source !== otaSource;
  const { status: sourceStatus, call: setOTA } = useAsyncCall(changeOTASource);

  useEffect(() => {
    if (otaSource) {
      setSource(otaSource);
    }
  }, [otaSource]);

  const handleSourceChange = useCallback((e: ChangeEvent<HTMLInputElement>) => {
    const { target } = e;
    const value = target.value.trim();
    setSource(value.startsWith('~') ? value : `~${value}`);
  }, []);

  const onSubmit = useCallback(
    (e: FormEvent<HTMLFormElement>) => {
      e.preventDefault();
      setOTA(source);
    },
    [source]
  );

  return (
    <>
      <h2 className="h3 mb-7">System Updates</h2>
      <div className="space-y-3">
        <Setting on={!!otasEnabled} toggle={toggleBase} name="Enable Automatic Urbit OTAs">
          <p>Automatically download and apply system updates to keep your Urbit up to date.</p>
          {otaSource && (
            <p>
              OTA Source: <ShipName name={otaSource} className="font-semibold font-mono" />
            </p>
          )}
        </Setting>
        <form className="inner-section relative" onSubmit={onSubmit}>
          <label htmlFor="ota-source" className="h4 mb-3">
            Switch OTA Source
          </label>
          <p className="mb-2">
            Enter a valid urbit name into this form to change who you receive OTA updates from. Be
            sure to select a reliable urbit!
          </p>
          <div className="relative">
            <input
              id="ota-source"
              type="text"
              value={source}
              onChange={handleSourceChange}
              className="input font-semibold default-ring"
            />
            {sourceDirty && (
              <Button type="submit" className="absolute top-1 right-1 py-1 px-3 text-sm">
                {sourceStatus !== 'loading' && 'Save'}
                {sourceStatus === 'loading' && (
                  <>
                    <span className="sr-only">Saving...</span>
                    <Spinner className="w-5 h-5" />
                  </>
                )}
              </Button>
            )}
          </div>
        </form>
      </div>
    </>
  );
};
