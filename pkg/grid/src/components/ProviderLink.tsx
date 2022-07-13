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
  adjustBG?: boolean;
};

export const ProviderLink = ({
  provider,
  to,
  selected = false,
  size = 'default',
  className,
  adjustBG,
  ...props
}: ProviderLinkProps) => {
  const small = size === 'small' || size === 'xs';
  return (
    <Link
      to={(to && to(provider)) || `/leap/search/${provider.shipName}/apps`}
      className={classNames(
        'flex items-center p-2 space-x-3 default-ring rounded-lg',
        !small && 'ring-offset-2',
        selected && 'bg-blue-200',
        className
      )}
      {...props}
    >
      <Avatar size={size} shipName={provider.shipName} adjustBG={adjustBG} />
      <div className="flex-1 text-black">
        <div className="flex font-mono space-x-4">
          <ShipName name={provider.shipName} />
          <span className="text-gray-500">{provider.nickname}</span>
        </div>
        {provider.status && size === 'default' && <p className="font-normal">{provider.status}</p>}
      </div>
    </Link>
  );
};
