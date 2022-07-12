import React from 'react';
import cn from 'classnames';
import { Notification, harkBinToId, HarkContent, HarkLid } from '@urbit/api';
import { map, take } from 'lodash';
import { useCharge } from '../../state/docket';
import { Elbow } from '../../components/icons/Elbow';
import { ShipName } from '../../components/ShipName';
import { DeskLink } from '../../components/DeskLink';
import { useHarkStore } from '../../state/hark';
import { DocketImage } from '../../components/DocketImage';
import { Button } from '../../components/Button';

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
  const orderedByTime = notification.body.sort((a, b) => a.time - b.time);
  const contents = map(orderedByTime, 'content').filter((c) => c.length > 0);
  const large = contents.length === 0;
  const archive = () => {
    useHarkStore.getState().archiveNote(notification.bin, lid);
  };

  const archiveNoFollow = (e: React.MouseEvent<HTMLButtonElement>) => {
    e.preventDefault();
    e.stopPropagation();
    archive();
  };

  return (
    <DeskLink
      onClick={archive}
      to={`?grid-note=${encodeURIComponent(first.link)}`}
      desk={desk}
      className={cn(
        'text-black rounded-xl group',
        'unseen' in lid ? 'bg-blue-100' : 'bg-gray-50',
        large ? 'note-grid-no-content' : 'note-grid-content'
      )}
      aria-labelledby={id}
    >
      <header id={id} className="contents">
        <DocketImage {...charge} size={!large ? 'xs' : 'default'} className="note-grid-icon" />
        <div className="font-semibold note-grid-title">{charge?.title || desk}</div>
        {!large ? <Elbow className="w-6 h-6 text-gray-300 note-grid-arrow" /> : null}
        <h2
          id={`${id}-title`}
          className="font-semibold leading-tight text-gray-600 note-grid-head sm:leading-normal"
        >
          <NotificationText contents={first.title} />
        </h2>
        {!('time' in lid) ? (
          <div className="flex self-center justify-center note-grid-actions sm:hidden hover-none:flex pointer-coarse:flex group-hover:flex">
            <Button
              onClick={archiveNoFollow}
              className="px-2 py-1 text-sm leading-none sm:px-4 sm:py-2 sm:text-base sm:leading-normal"
            >
              Archive
            </Button>
          </div>
        ) : null}
      </header>
      {contents.length > 0 ? (
        <div className="leading-tight note-grid-body sm:leading-normal space-y-2">
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
