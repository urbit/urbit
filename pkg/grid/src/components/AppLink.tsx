import classNames from 'classnames';
import React, { HTMLProps, ReactNode } from 'react';
import { Link, LinkProps } from 'react-router-dom';
import { DocketWithDesk } from '../state/docket';
import { getAppHref } from '../state/util';
import { DocketImage } from './DocketImage';

type Sizes = 'xs' | 'small' | 'default';
type LinkOrAnchorProps = {
  [P in keyof LinkProps &
    keyof HTMLProps<HTMLAnchorElement>]?: LinkProps[P] extends HTMLProps<HTMLAnchorElement>[P]
    ? LinkProps[P]
    : never;
};

export type AppLinkProps<T extends DocketWithDesk> = Omit<LinkOrAnchorProps, 'to'> & {
  app: T;
  size?: Sizes;
  selected?: boolean;
  to?: (app: T) => LinkProps['to'] | undefined;
};

export const AppLink = <T extends DocketWithDesk>({
  app,
  to,
  size = 'default',
  selected = false,
  className,
  ...props
}: AppLinkProps<T>) => {
  const linkTo = to?.(app);
  const linkClassnames = classNames(
    'flex items-center default-ring rounded-lg',
    size === 'default' && 'ring-offset-2',
    size !== 'xs' && 'p-2',
    size === 'xs' && 'p-1',
    selected && 'bg-blue-200',
    className
  );
  const link = (children: ReactNode) =>
    linkTo ? (
      <Link to={linkTo} className={linkClassnames} {...props}>
        {children}
      </Link>
    ) : (
      <a href={getAppHref(app.href)} target={app.desk} className={linkClassnames} {...props}>
        {children}
      </a>
    );
  return link(
    <>
      <DocketImage color={app.color} image={app.image} size={size} />
      <div className="flex-1 text-black">
        <p>{app.title}</p>
        {app.info && size === 'default' && <p className="font-normal">{app.info}</p>}
      </div>
    </>
  );
};
