import classNames from 'classnames';
import React from 'react';
import { Link, LinkProps } from 'react-router-dom';
import { Provider } from '../state/docket-types';
import { ShipName } from './ShipName';

export type ProviderLinkProps = Omit<LinkProps, 'to'> & {
  provider: Provider;
  small?: boolean;
  selected?: boolean;
  to?: (p: Provider) => LinkProps['to'];
};

export const ProviderLink = ({
  provider,
  to,
  selected = false,
  small = false,
  className,
  ...props
}: ProviderLinkProps) => {
  return (
    <Link
      to={(to && to(provider)) || `/leap/search/${provider.shipName}/apps`}
      className={classNames(
        'flex items-center space-x-3 default-ring ring-offset-2 rounded-lg',
        selected && 'ring-4',
        className
      )}
      {...props}
    >
      <div
        className={classNames(
          'flex-none relative bg-black rounded-lg',
          small ? 'w-8 h-8' : 'w-12 h-12'
        )}
      >
        {/* TODO: Handle sigils */}
      </div>
      <div className="flex-1 text-black">
        <p className="font-mono">{provider.nickname || <ShipName name={provider.shipName} />}</p>
        {provider.status && !small && <p className="font-normal">{provider.status}</p>}
      </div>
    </Link>
  );
};
