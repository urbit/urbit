import React, { useCallback } from 'react';
import { useMutation, useQuery, useQueryClient } from 'react-query';
import { Redirect, useHistory, useParams } from 'react-router-dom';
import { Button } from '../components/Button';
import { Dialog, DialogContent } from '../components/Dialog';
import { chargesKey, toggleDocket } from '../state/docket';
import { Docket } from '../state/docket-types';

export const SuspendApp = () => {
  const queryClient = useQueryClient();
  const history = useHistory();
  const { desk } = useParams<{ desk: string }>();
  const { data: docket } = useQuery<Docket>(chargesKey([desk]));
  const { mutate } = useMutation(() => toggleDocket(desk), {
    onSuccess: () => {
      history.push('/');
      queryClient.invalidateQueries(chargesKey());
    }
  });

  // TODO: add optimistic updates
  const handleSuspendApp = useCallback(() => mutate(), []);

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
