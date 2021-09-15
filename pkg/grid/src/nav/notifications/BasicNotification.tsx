import React from 'react';
import cn from 'classnames';
import { Notification, harkBinToId, HarkContent } from '@urbit/api';
import { map, take } from 'lodash';
import { useCharge } from '../../state/docket';
import { Elbow } from '../../components/icons/Elbow';
import { ShipName } from '../../components/ShipName';
import { getAppHref } from '../../state/util';
import { DocketImage } from '../../components/DocketImage';

interface BasicNotificationProps {
  notification: Notification;
  unread?: boolean;
}

const MAX_CONTENTS = 20;

const NotificationText = ({ contents }: { contents: HarkContent[] }) => {
  return (
    <>
      {contents.map((content, idx) => {
        if ('ship' in content) {
          return <ShipName key={idx} name={content.ship} />;
        }
        return content.text;
      })}
    </>
  );
};

export const BasicNotification = ({ notification, unread = false }: BasicNotificationProps) => {
  const { desk } = notification.bin.place;
  const binId = harkBinToId(notification.bin);
  const id = `notif-${notification.time}-${binId}`;

  const charge = useCharge(desk);
  const first = notification.body?.[0];
  if (!first || !charge) {
    return null;
  }
  const contents = map(notification.body, 'content').filter((c) => c.length > 0);
  const large = contents.length === 0;
  const link = `${getAppHref(charge.href)}?grid-note=${encodeURIComponent(first.link)}`;

  return (
    <a
      href={link}
      target={desk}
      className={cn(
        'text-black rounded-xl',
        unread ? 'bg-blue-100' : 'bg-gray-100',
        large ? 'note-grid-no-content' : 'note-grid-content'
      )}
      aria-labelledby={id}
    >
      <header id={id} className="contents">
        <DocketImage {...charge} size={!large ? 'xs' : 'default'} className="note-grid-icon" />
        <div className="note-grid-title font-semibold">{charge?.title || desk}</div>
        {!large ? <Elbow className="note-grid-arrow w-6 h-6 text-gray-300" /> : null}
        <h2 id={`${id}-title`} className="note-grid-head font-semibold text-gray-600">
          <NotificationText contents={first.title} />
        </h2>
      </header>
      {contents.length > 0 ? (
        <div className="note-grid-body space-y-2">
          {take(contents, MAX_CONTENTS).map((content) => (
            <p className="">
              <NotificationText contents={content} />
            </p>
          ))}
          {contents.length > MAX_CONTENTS ? (
            <p className="text-gray-300">and {contents.length - MAX_CONTENTS} more</p>
          ) : null}
        </div>
      ) : null}
    </a>
  );
};
