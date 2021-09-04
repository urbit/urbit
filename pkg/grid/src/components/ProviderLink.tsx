import classNames from 'classnames';
import React from 'react';
import { Link, LinkProps } from 'react-router-dom';
import { Contact, Provider } from '@urbit/api';
import { ShipName } from './ShipName';
import { Avatar, AvatarSizes } from './Avatar';

export type ProviderLinkProps = Omit<LinkProps, 'to'> & {
  provider: { shipName: string } & Contact;
  size?: AvatarSizes;
  selected?: boolean;
  to?: (p: Provider) => LinkProps['to'];
};

export const ProviderLink = ({
  provider,
  to,
  selected = false,
  size = 'default',
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
      <Avatar size={size} {...provider} />
      <div className="flex-1 text-black">
        <p className="font-mono">{provider.nickname || <ShipName name={provider.shipName} />}</p>
        {provider.status && size === 'default' && <p className="font-normal">{provider.status}</p>}
      </div>
    </Link>
  );
};
