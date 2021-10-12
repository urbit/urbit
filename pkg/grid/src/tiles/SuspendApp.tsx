import React, { useCallback } from 'react';
import { Redirect, useHistory, useParams } from 'react-router-dom';
import { Button } from '../components/Button';
import { Dialog, DialogClose, DialogContent } from '../components/Dialog';
import { useRecentsStore } from '../nav/search/Home';
import useDocketState, { useCharges } from '../state/docket';

export const SuspendApp = () => {
  const history = useHistory();
  const { desk } = useParams<{ desk: string }>();
  const charges = useCharges();
  const charge = charges[desk];

  // TODO: add optimistic updates
  const handleSuspendApp = useCallback(() => {
    useDocketState.getState().toggleDocket(desk);
    useRecentsStore.getState().removeRecentApp(desk);
  }, [desk]);

  if ('suspend' in charge.chad) {
    return <Redirect to="/" />;
  }

  return (
    <Dialog open onOpenChange={(open) => !open && history.push('/')}>
      <DialogContent showClose={false} className="space-y-6" containerClass="w-full max-w-md">
        <h1 className="h4">Suspend &ldquo;{charge?.title || ''}&rdquo;</h1>
        <p className="text-base tracking-tight pr-6">
          Suspending an app will turn off automatic updates. You cannot use an app when it is
          suspended, but you can resume it at any time.
        </p>
        <div className="flex space-x-6">
          <DialogClose as={Button} variant="secondary">
            Cancel
          </DialogClose>
          <DialogClose as={Button} onClick={handleSuspendApp}>
            Suspend &ldquo;{charge?.title}&rdquo;
          </DialogClose>
        </div>
      </DialogContent>
    </Dialog>
  );
};
