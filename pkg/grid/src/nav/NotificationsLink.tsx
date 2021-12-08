import classNames from 'classnames';
import React, { useCallback } from 'react';
import { Timebox } from '@urbit/api';
import { Link, LinkProps } from 'react-router-dom';
import { Bullet } from '../components/icons/Bullet';
import { Cross } from '../components/icons/Cross';
import { useHarkStore } from '../state/hark';
import { useLeapStore } from './Nav';

type NotificationsState = 'empty' | 'unread' | 'attention-needed' | 'open';

function getNotificationsState(isOpen: boolean, box: Timebox): NotificationsState {
  const notifications = Object.values(box);
  if (
    notifications.filter(
      ({ bin }) => bin.place.desk === window.desk && ['/lag', 'blocked'].includes(bin.place.path)
    ).length > 0
  ) {
    return 'attention-needed';
  }
  if (isOpen) {
    return 'open';
  }

  // TODO: when real structure, this should be actually be unread not just existence
  if (notifications.length > 0) {
    return 'unread';
  }

  return 'empty';
}

type NotificationsLinkProps = Omit<LinkProps<HTMLAnchorElement>, 'to'> & {
  navOpen: boolean;
  notificationsOpen: boolean;
  shouldDim: boolean;
};

export const NotificationsLink = ({
  navOpen,
  notificationsOpen,
  shouldDim
}: NotificationsLinkProps) => {
  const unseen = useHarkStore((s) => s.unseen);
  const state = getNotificationsState(notificationsOpen, unseen);
  const select = useLeapStore((s) => s.select);
  const clearSelection = useCallback(() => select(null), [select]);

  return (
    <Link
      to={state === 'open' ? '/' : '/leap/notifications'}
      className={classNames(
        'relative z-50 flex-none circle-button h4 default-ring',
        navOpen && 'text-opacity-60',
        shouldDim && 'opacity-60',
        state === 'open' && 'text-gray-400 bg-white',
        state === 'empty' && !navOpen && 'text-gray-400 bg-gray-50',
        state === 'empty' && navOpen && 'text-gray-400 bg-white',
        state === 'unread' && 'bg-blue-400 text-white',
        state === 'attention-needed' && 'text-white bg-orange-400'
      )}
      onClick={clearSelection}
    >
      {state === 'empty' && <Bullet className="w-6 h-6" />}
      {state === 'unread' && Object.keys(unseen).length}
      {state === 'attention-needed' && (
        <span className="h2">
          ! <span className="sr-only">Attention needed</span>
        </span>
      )}
      {state === 'open' && (
        <>
          <Cross className="w-3 h-3 fill-current" />
          <span className="sr-only">Close</span>
        </>
      )}
    </Link>
  );
};
