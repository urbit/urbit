import { Vat } from '@urbit/api';
import _ from 'lodash';
import React, { ChangeEvent, FormEvent, useCallback, useEffect, useState } from 'react';
import { Button } from '../../components/Button';
import { Setting } from '../../components/Setting';
import { Spinner } from '../../components/Spinner';
import { useAsyncCall } from '../../logic/useAsyncCall';
import useKilnState from '../../state/kiln';

interface UpdatePreferencesProps {
  base: Vat | undefined;
}

export const UpdatePreferences = ({ base }: UpdatePreferencesProps) => {
  const { changeOTASource, toggleOTAs } = useKilnState((s) =>
    _.pick(s, ['toggleOTAs', 'changeOTASource'])
  );
  const otasEnabled = base && !(base.arak?.rail?.paused ?? true);
  const otaSource = base && base.arak.rail?.ship;

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
    <div className="inner-section space-y-8 relative">
      <h2 className="h4">Update Preferences</h2>
      <form onSubmit={onSubmit}>
        <label htmlFor="ota-source" className="block font-semibold mb-1.5">
          System Update Provider
        </label>
        <div className="flex items-center space-x-2">
          <input
            id="ota-source"
            type="text"
            value={source}
            onChange={handleSourceChange}
            className="input bg-gray-50 font-semibold default-ring"
          />
          <Button type="submit" disabled={!sourceDirty}>
            {sourceStatus !== 'loading' && 'Update'}
            {sourceStatus === 'loading' && (
              <>
                <span className="sr-only">Saving...</span>
                <Spinner className="w-5 h-5" />
              </>
            )}
          </Button>
        </div>
      </form>
      <Setting on={!!otasEnabled} toggle={toggleBase} name="Automatically Update My Urbit">
        <p className="text-gray-600 leading-5">
          Ensure that system updates are downloaded and applied as soon as my update provider has an
          update readied
        </p>
      </Setting>
    </div>
  );
};
