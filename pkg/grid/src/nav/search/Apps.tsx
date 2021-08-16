import React, { useCallback, useEffect } from 'react';
import { useQuery, useQueryClient } from 'react-query';
import { Link, RouteComponentProps } from 'react-router-dom';
import fuzzy from 'fuzzy';
import slugify from 'slugify';
import { ShipName } from '../../components/ShipName';
import { fetchProviderTreaties, treatyKey } from '../../state/docket';
import { Treaty } from '../../state/docket-types';
import { useNavStore } from '../Nav';

type AppsProps = RouteComponentProps<{ ship: string }>;

export const Apps = ({ match }: AppsProps) => {
  const queryClient = useQueryClient();
  const { searchInput, select } = useNavStore((state) => ({
    searchInput: state.searchInput,
    select: state.select
  }));
  const provider = match?.params.ship;
  const { data } = useQuery(treatyKey([provider]), () => fetchProviderTreaties(provider), {
    enabled: !!provider
  });
  const results = data
    ? fuzzy
        .filter(
          searchInput,
          data.map((t) => t.title)
        )
        .map((result) => data[result.index])
    : undefined;
  const count = results?.length;

  useEffect(() => {
    select(
      <>
        Apps by <ShipName name={provider} className="font-mono" />
      </>
    );
  }, []);

  const preloadApp = useCallback(
    (app: Treaty) => {
      queryClient.setQueryData(treatyKey([provider, app.desk]), app);
    },
    [queryClient]
  );

  return (
    <div className="dialog-inner-container md:px-6 md:py-8 h4 text-gray-400">
      <div id="developed-by">
        <h2 className="mb-3">
          Software developed by <ShipName name={provider} className="font-mono" />
        </h2>
        <p>
          {count} result{count === 1 ? '' : 's'}
        </p>
      </div>
      {results && (
        <ul className="space-y-8" aria-labelledby="developed-by">
          {results.map((app) => (
            <li key={app.desk}>
              <Link
                to={`${match?.path.replace(':ship', provider)}/${slugify(app.desk)}`}
                className="flex items-center space-x-3 default-ring ring-offset-2 rounded-lg"
                onClick={() => preloadApp(app)}
              >
                <div
                  className="flex-none relative w-12 h-12 bg-gray-200 rounded-lg"
                  style={{ backgroundColor: app.color }}
                >
                  {app.img && (
                    <img
                      className="absolute top-1/2 left-1/2 h-[40%] w-[40%] object-contain transform -translate-x-1/2 -translate-y-1/2"
                      src={app.img}
                      alt=""
                    />
                  )}
                </div>
                <div className="flex-1 text-black">
                  <p>{app.title}</p>
                  {app.info && <p className="font-normal">{app.info}</p>}
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
