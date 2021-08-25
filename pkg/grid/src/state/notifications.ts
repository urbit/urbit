import { useHarkStore } from './hark';
import { Notification } from './hark-types';
import { useBlockers, useLag } from './kiln';

function getSystemNotifications(lag: boolean, blockers: string[]) {
  const nots = [] as Notification[];
  if (lag) {
    nots.push({ type: 'runtime-lag' });
  }
  if (blockers.length > 0) {
    nots.push({ type: 'system-updates-blocked', desks: blockers });
  }
  return nots;
}

export const useNotifications = () => {
  const notifications = useHarkStore((s) => s.notifications);
  const blockers = useBlockers();
  const lag = useLag();
  const systemNotifications = getSystemNotifications(lag, blockers);
  const hasAnyNotifications = notifications.length > 0 || systemNotifications.length > 0;

  return {
    notifications,
    systemNotifications,
    hasAnyNotifications
  };
};
