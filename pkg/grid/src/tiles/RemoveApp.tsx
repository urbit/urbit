import React, { useCallback } from 'react';
import { useHistory, useParams } from 'react-router-dom';
import { Button } from '../components/Button';
import { Dialog, DialogClose, DialogContent } from '../components/Dialog';
import { useRecentsStore } from '../nav/search/Home';
import useDocketState, { useCharges } from '../state/docket';
import { getAppName } from '../state/util';

export const RemoveApp = () => {
  const history = useHistory();
  const { desk } = useParams<{ desk: string }>();
  const charges = useCharges();
  const docket = charges[desk];
  const uninstallDocket = useDocketState((s) => s.uninstallDocket);

  // TODO: add optimistic updates
  const handleRemoveApp = useCallback(() => {
    uninstallDocket(desk);
    useRecentsStore.getState().removeRecentApp(desk);
  }, [desk]);

  return (
    <Dialog open onOpenChange={(open) => !open && history.push('/')}>
      <DialogContent showClose={false} className="space-y-6" containerClass="w-full max-w-md">
        <h1 className="h4">Uninstall &ldquo;{getAppName(docket)}&rdquo;?</h1>
        <p className="tracking-tight pr-6">
          The app tile will be removed from Landscape, all processes will be stopped and their data
          archived, and the app will stop receiving updates.
        </p>
        <p className="tracking-tight pr-6">
          If the app is reinstalled, the archived data will be restored and you&apos;ll be able to
          pick up where you left off.
        </p>
        <div className="flex space-x-6">
          <DialogClose as={Button} variant="secondary">
            Cancel
          </DialogClose>
          <DialogClose as={Button} onClick={handleRemoveApp}>
            Uninstall
          </DialogClose>
        </div>
      </DialogContent>
    </Dialog>
  );
};
