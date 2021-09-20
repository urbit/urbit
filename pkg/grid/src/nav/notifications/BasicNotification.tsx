import React from 'react';
import cn from 'classnames';
import { Notification, harkBinToId, HarkContent, HarkLid } from '@urbit/api';
import { map, take } from 'lodash';
import { useCharge } from '../../state/docket';
import { Elbow } from '../../components/icons/Elbow';
import { ShipName } from '../../components/ShipName';
import { getAppHref } from '../../state/util';
import { Link } from 'react-router-dom';
import { DeskLink } from '../../components/DeskLink';
import {useHarkStore} from '../../state/hark';

interface BasicNotificationProps {
  notification: Notification;
  lid: HarkLid;
}

const MAX_CONTENTS = 5;

const NotificationText = ({ contents }: { contents: HarkContent[] }) => {
  return (
    <>
      {contents.map((content, idx) => {
        if ('ship' in content) {
          return <ShipName className="color-blue" key={idx} name={content.ship} />;
        }
        return content.text;
      })}
    </>
  );
};

export const BasicNotification = ({ notification, lid }: BasicNotificationProps) => {
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
  const archive = () => {
    const { bin } = notification;
    useHarkStore.getState().archiveNote(notification.bin, lid);
  };

  return (
    <DeskLink
      onClick={archive}
      to={`?grid-note=${encodeURIComponent(first.link)}`}
      desk={desk}
      className={cn(
        'text-black rounded',
        'unseen' in lid ? 'bg-blue-100' : 'bg-gray-100',
        large ? 'note-grid-no-content' : 'note-grid-content'
      )}
      aria-labelledby={id}
    >
      <header id={id} className="contents">
        <div
          className="note-grid-icon rounded w-full h-full"
          style={{ backgroundColor: charge?.color ?? '#ee5432' }}
        />
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
    </DeskLink>
  );
};
