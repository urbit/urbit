import React, { useCallback } from 'react';
import { useMutation, useQuery, useQueryClient } from 'react-query';
import { useHistory, useParams } from 'react-router-dom';
import { Button } from '../components/Button';
import { Dialog, DialogContent } from '../components/Dialog';
import { chargesKey, uninstallDocket } from '../state/docket';
import { Docket } from '../state/docket-types';

export const RemoveApp = () => {
  const queryClient = useQueryClient();
  const history = useHistory();
  const { desk } = useParams<{ desk: string }>();
  const { data: docket } = useQuery<Docket>(chargesKey([desk]));
  const { mutate } = useMutation(() => uninstallDocket(desk), {
    onSuccess: () => {
      history.push('/');
      queryClient.invalidateQueries(chargesKey());
    }
  });

  // TODO: add optimistic updates
  const handleRemoveApp = useCallback(() => {
    mutate();
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
