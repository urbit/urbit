import classNames from 'classnames';
import React from 'react';
import { Notification } from '@urbit/api';
import { Link, LinkProps } from 'react-router-dom';
import { Bullet } from '../components/icons/Bullet';
import { useNotifications } from '../state/notifications';

type NotificationsState = 'empty' | 'unread' | 'attention-needed';

function getNotificationsState(
  notifications: Notification[],
  systemNotifications: Notification[]
): NotificationsState {
  if (systemNotifications.length > 0) {
    return 'attention-needed';
  }

  // TODO: when real structure, this should be actually be unread not just existence
  if (notifications.length > 0) {
    return 'unread';
  }

  return 'empty';
}

type NotificationsLinkProps = Omit<LinkProps<HTMLAnchorElement>, 'to'> & {
  isOpen: boolean;
};

export const NotificationsLink = ({ isOpen }: NotificationsLinkProps) => {
  const { unreads, systemNotifications } = useNotifications();
  const state = getNotificationsState(unreads, systemNotifications);

  return (
    <Link
      to="/leap/notifications"
      className={classNames(
        'relative z-50 flex-none circle-button h4',
        isOpen && 'text-opacity-60',
        state === 'empty' && !isOpen && 'text-gray-400 bg-gray-100',
        state === 'empty' && isOpen && 'text-gray-400 bg-white',
        state === 'unread' && 'bg-blue-400 text-white',
        state === 'attention-needed' && 'text-white bg-orange-500'
      )}
    >
      {state === 'empty' && <Bullet className="w-6 h-6" />}
      {state === 'unread' && unreads.length}
      {state === 'attention-needed' && (
        <span className="h2">
          ! <span className="sr-only">Attention needed</span>
        </span>
      )}
    </Link>
  );
};
