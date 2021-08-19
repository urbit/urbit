import React, { useCallback } from 'react';
import { Redirect, useHistory, useParams } from 'react-router-dom';
import { Button } from '../components/Button';
import { Dialog, DialogContent } from '../components/Dialog';
import useDocketState, { useCharges } from '../state/docket';

export const SuspendApp = () => {
  const history = useHistory();
  const { desk } = useParams<{ desk: string }>();
  const charges = useCharges();
  const docket = charges[desk];
  const toggleDocket = useDocketState((s) => s.toggleDocket);

  // TODO: add optimistic updates
  const handleSuspendApp = useCallback(() => {
    toggleDocket(desk);
    history.push('/');
  }, []);

  if (docket?.status === 'suspended') {
    <Redirect to="/" />;
  }

  return (
    <Dialog open onOpenChange={(open) => !open && history.push('/')}>
      <DialogContent>
        <h1 className="h4 mb-9">Suspend &ldquo;{docket?.title || ''}&rdquo;</h1>
        <p className="text-base tracking-tight mb-4 pr-6">
          Suspending an app will freeze its current state, and render it unable
        </p>
        <Button variant="destructive" onClick={handleSuspendApp}>
          Suspend
        </Button>
      </DialogContent>
    </Dialog>
  );
};
