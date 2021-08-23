import React, { useCallback } from 'react';
import { useHistory, useParams } from 'react-router-dom';
import { Button } from '../components/Button';
import { Dialog, DialogClose, DialogContent } from '../components/Dialog';
import useDocketState, { useCharges } from '../state/docket';

export const RemoveApp = () => {
  const history = useHistory();
  const { desk } = useParams<{ desk: string }>();
  const charges = useCharges();
  const docket = charges[desk];
  const uninstallDocket = useDocketState((s) => s.uninstallDocket);

  // TODO: add optimistic updates
  const handleRemoveApp = useCallback(() => {
    uninstallDocket(desk);
  }, [desk]);

  return (
    <Dialog open onOpenChange={(open) => !open && history.push('/')}>
      <DialogContent showClose={false} className="max-w-[400px] space-y-6">
        <h1 className="h4">Remove &ldquo;{docket?.title || ''}&rdquo;?</h1>
        <p className="text-base tracking-tight pr-6">
          This will remove the software&apos;s tile from your home screen.
        </p>
        <div className="flex space-x-6">
          <DialogClose as={Button} variant="secondary">
            Cancel
          </DialogClose>
          <DialogClose as={Button} onClick={handleRemoveApp}>
            Remove &ldquo;{docket?.title}&rdquo;
          </DialogClose>
        </div>
      </DialogContent>
    </Dialog>
  );
};
