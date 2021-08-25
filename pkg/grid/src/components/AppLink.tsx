import classNames from 'classnames';
import React from 'react';
import { Link, LinkProps } from 'react-router-dom';
import { Docket } from '@urbit/api';
import { getAppHref } from '../state/util';

type Sizes = 'xs' | 'small' | 'default';

export type AppLinkProps = Omit<LinkProps, 'to'> & {
  app: Docket;
  size?: Sizes;
  selected?: boolean;
  to?: (app: Docket) => LinkProps['to'];
};

const sizeMap: Record<Sizes, string> = {
  xs: 'w-6 h-6 mr-2 rounded',
  small: 'w-8 h-8 mr-3 rounded-lg',
  default: 'w-12 h-12 mr-3 rounded-lg'
};

export const AppLink = ({
  app,
  to,
  size = 'default',
  selected = false,
  className,
  ...props
}: AppLinkProps) => {
  return (
    <Link
      to={(to && to(app)) || getAppHref(app.href)}
      className={classNames(
        'flex items-center default-ring ring-offset-2 rounded-lg',
        selected && 'ring-4',
        className
      )}
      {...props}
    >
      <div
        className={classNames('flex-none relative bg-gray-200 rounded-lg', sizeMap[size])}
        style={{ backgroundColor: app.color }}
      >
        {app.image && (
          <img
            className="absolute top-1/2 left-1/2 h-[40%] w-[40%] object-contain transform -translate-x-1/2 -translate-y-1/2"
            src={app.image}
            alt=""
          />
        )}
      </div>
      <div className="flex-1 text-black">
        <p>{app.title}</p>
        {app.info && size === 'default' && <p className="font-normal">{app.info}</p>}
      </div>
    </Link>
  );
};
