import React, { useEffect } from 'react';
import { useQuery } from 'react-query';
import { Link, RouteComponentProps } from 'react-router-dom';
import { ShipName } from '../../components/ShipName';
import { fetchProviders, providersKey } from '../../state/docket';
import { useNavStore } from '../Nav';

type ProvidersProps = RouteComponentProps<{ ship: string }>;

export const Providers = ({ match }: ProvidersProps) => {
  const select = useNavStore((state) => state.select);
  const provider = match?.params.ship;
  const { data } = useQuery(providersKey([provider]), () => fetchProviders(provider), {
    enabled: !!provider,
    keepPreviousData: true
  });
  const count = data?.length;

  useEffect(() => {
    select(null, provider);
  }, []);

  return (
    <div className="dialog-inner-container md:px-6 md:py-8 h4 text-gray-400" aria-live="polite">
      <div id="providers">
        <h2 className="mb-3">Searching Software Providers</h2>
        <p>
          {count} result{count === 1 ? '' : 's'}
        </p>
      </div>
      {data && (
        <ul className="space-y-8" aria-labelledby="providers">
          {data.map((p) => (
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
