import { debounce } from 'lodash-es';
import React, { useCallback, useEffect, useState } from 'react';
import { Link, RouteComponentProps } from 'react-router-dom';
import { ShipName } from '../../components/ShipName';
import useDocketState from '../../state/docket';
import { Provider } from '../../state/docket-types';
import { useNavStore } from '../Nav';

type ProvidersProps = RouteComponentProps<{ ship: string }>;

export const Providers = ({ match, history }: ProvidersProps) => {
  const { searchInput, select } = useNavStore((state) => ({
    searchInput: state.searchInput,
    select: state.select
  }));
  const { push } = history;
  const { path } = match;
  const provider = match?.params.ship;
  const fetchProviders = useDocketState((s) => s.fetchProviders);
  const [providers, setProviders] = useState<Provider[]>();
  const count = providers?.length;

  useEffect(() => {
    async function getProviders() {
      setProviders(await fetchProviders(provider));
    }

    select(null, provider);
    getProviders();
  }, [provider]);

  const handleSearch = useCallback(
    debounce((input: string) => {
      push(match?.path.replace(':ship', input.trim()));
    }, 300),
    [path]
  );

  useEffect(() => {
    if (searchInput) {
      handleSearch(searchInput);
    }
  }, [searchInput]);

  return (
    <div className="dialog-inner-container md:px-6 md:py-8 h4 text-gray-400" aria-live="polite">
      <div id="providers">
        <h2 className="mb-3">Searching Software Providers</h2>
        <p>
          {count} result{count === 1 ? '' : 's'}
        </p>
      </div>
      {providers && (
        <ul className="space-y-8" aria-labelledby="providers">
          {providers.map((p) => (
            <li key={p.shipName}>
              <Link
                to={`${match?.path.replace(':ship', p.shipName)}/apps`}
                className="flex items-center space-x-3 default-ring ring-offset-2 rounded-lg"
              >
                <div className="flex-none relative w-12 h-12 bg-black rounded-lg">
                  {/* TODO: Handle sigils */}
                </div>
                <div className="flex-1 text-black">
                  <p className="font-mono">{p.nickname || <ShipName name={p.shipName} />}</p>
                  {p.status && <p className="font-normal">{p.status}</p>}
                </div>
              </Link>
            </li>
          ))}
        </ul>
      )}
      <p>That&apos;s it!</p>
    </div>
  );
};
