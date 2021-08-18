import classNames from 'classnames';
import React from 'react';
import { Link, LinkProps } from 'react-router-dom';
import { Docket } from '../state/docket-types';

export type AppLinkProps = Omit<LinkProps, 'to'> & {
  app: Docket;
  small?: boolean;
  selected?: boolean;
  to?: (app: Docket) => LinkProps['to'];
};

export const AppLink = ({
  app,
  to,
  small = false,
  selected = false,
  className,
  ...props
}: AppLinkProps) => {
  return (
    <Link
      to={(to && to(app)) || `/apps/${app.base}`}
      className={classNames(
        'flex items-center space-x-3 default-ring ring-offset-2 rounded-lg',
        selected && 'ring-4',
        className
      )}
      {...props}
    >
      <div
        className={classNames(
          'flex-none relative bg-gray-200 rounded-lg',
          small ? 'w-8 h-8' : 'w-12 h-12'
        )}
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
        {app.info && !small && <p className="font-normal">{app.info}</p>}
      </div>
    </Link>
  );
};
