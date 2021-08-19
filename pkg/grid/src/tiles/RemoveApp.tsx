import React, { useCallback } from 'react';
import { useHistory, useParams } from 'react-router-dom';
import { Button } from '../components/Button';
import { Dialog, DialogContent } from '../components/Dialog';
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
    history.push('/');
  }, []);

  return (
    <Dialog open onOpenChange={(open) => !open && history.push('/')}>
      <DialogContent>
        <h1 className="h4 mb-9">Remove &ldquo;{docket?.title || ''}&rdquo;</h1>
        <p className="text-base tracking-tight mb-4 pr-6">
          Explanatory writing about what data will be kept.
        </p>
        <Button variant="destructive" onClick={handleRemoveApp}>
          Remove
        </Button>
      </DialogContent>
    </Dialog>
  );
};
