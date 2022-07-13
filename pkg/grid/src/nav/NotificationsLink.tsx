import classNames from 'classnames';
import React, { useCallback } from 'react';
import { Timebox } from '@urbit/api';
import { Link, LinkProps } from 'react-router-dom';
import { Cross } from '../components/icons/Cross';
import { useHarkStore } from '../state/hark';
import { useLeapStore } from './Nav';
import { SettingsState, useSettingsState } from '../state/settings';
import BellIcon from '../components/icons/BellIcon';

type NotificationsState = 'empty' | 'unread' | 'attention-needed' | 'open';

function getNotificationsState(isOpen: boolean, box: Timebox, dnd: boolean): NotificationsState {
  const notifications = Object.values(box);
  if (isOpen) {
    return 'open';
  }

  if (dnd) {
    return 'empty';
  }

  if (
    notifications.filter(
      ({ bin }) => bin.place.desk === window.desk && ['/lag', 'blocked'].includes(bin.place.path)
    ).length > 0
  ) {
    return 'attention-needed';
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
};

const selDnd = (s: SettingsState) => s.display.doNotDisturb;

export const NotificationsLink = ({ navOpen, notificationsOpen }: NotificationsLinkProps) => {
  const unseen = useHarkStore((s) => s.unseen);
  const dnd = useSettingsState(selDnd);
  const state = getNotificationsState(notificationsOpen, unseen, dnd);
  const select = useLeapStore((s) => s.select);
  const clearSelection = useCallback(() => select(null), [select]);

  return (
    <Link
      to={state === 'open' ? '/' : '/leap/notifications'}
      className={classNames(
        'relative z-50 flex-none circle-button h4 default-ring',
        navOpen && 'text-opacity-60',
        state === 'open' && 'text-gray-400 bg-white',
        state === 'empty' && !navOpen && 'text-gray-400 bg-gray-50',
        state === 'empty' && navOpen && 'text-gray-400 bg-white',
        state === 'unread' && 'bg-blue-400 text-white',
        state === 'attention-needed' && 'text-white bg-orange-400'
      )}
      onClick={clearSelection}
    >
      {state === 'empty' && <BellIcon className="w-6 h-6" />}
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
