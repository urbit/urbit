import React, { FC, useEffect, useState } from 'react';
import cn from 'classnames';
import { useLeapStore } from './Nav';
import { useBlockers, useLag } from '../state/kiln';
import { useCharge } from '../state/docket';
import { DocketImage } from '../components/DocketImage';
import api from '../state/api';
import { kilnSuspend } from '../../../npm/api/hood';

interface INotification {
  title: string;
  body: string;
  actions: {
    title: string;
    role: 'primary' | 'destructive' | 'none';
    link: string;
  }[];
}

interface NotificationProps {
  notification?: INotification;
  read?: boolean;
  className?: string;
  children?: React.ReactNode;
}

const Notification: FC<NotificationProps> = ({ notification, className, children }) => (
  <div className={cn('rounded-md flex flex-col p-4', className)}>{children}</div>
);

const LagNotification = () => (
  <Notification read={false} className="bg-orange-100">
    <p className="text-black leading-normal">
      The runtime of this ship is out of date, and preventing a kernel upgrade. Please upgrade to
      the latest runtime version. If you are hosted, please contact your hosting provider
    </p>
  </Notification>
);

const DeskIcon = ({ desk }) => {
  const { title, image, color } = useCharge(desk);

  return (
    <div className="flex items-center space-x-2">
      <DocketImage small color={color} image={image} />
      <p className="text-black font-medium">{title}</p>
    </div>
  );
};

interface BlockNotificationProps {
  desks: string[];
}
const BlockNotification: React.FC<BlockNotificationProps> = ({ desks }) => {
  const count = desks.length;
  const [dismissed, setDismissed] = useState(false);
  const onArchive = async () => {
    await Promise.all(desks.map((d) => api.poke(kilnSuspend(d))));
  };

  if (dismissed) {
    return null;
  }
  return (
    <Notification className="bg-orange-100 flex-col space-y-4 p-6">
      <p className="text-black"> The following {desks.length} apps blocked a System Update: </p>
      <div className="flex flex-col space-y-4">
        {desks.map((desk) => (
          <DeskIcon key={desk} desk={desk} />
        ))}
      </div>
      <p className="text-black">
        In order to proceed with the System Update, you’ll need to temporarily archive these apps,
        which will render them unusable, but with data intact.
        <br />
        <br />
        Archived apps will automatically un-archive and resume operation when their developer
        provides an app update.
      </p>
      <div className="flex space-x-4">
        <button onClick={() => setDismissed(true)}>Dismiss</button>
        <button onClick={onArchive}>Archive {count} apps and System Update</button>
      </div>
    </Notification>
  );
};

export const Notifications = () => {
  const select = useLeapStore((state) => state.select);

  useEffect(() => {
    select('Notifications');
  }, []);

  const blockers = useBlockers();
  const lag = useLag();

  return (
    <div className="p-4 md:p-8 space-y-8">
      {lag && <LagNotification />}
      {blockers.length > 0 ? <BlockNotification desks={blockers} /> : null}
      {/*<h2 className="h4 text-gray-500">Recent Apps</h2>
      <div className="min-h-[150px] rounded-xl bg-gray-100" />
      <hr className="-mx-4 md:-mx-8" />
      <h2 className="h4 text-gray-500">Recent Developers</h2>
      <div className="min-h-[150px] rounded-xl bg-gray-100" />
        */}
    </div>
  );
};
