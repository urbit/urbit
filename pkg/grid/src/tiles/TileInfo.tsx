import React from 'react';
import { useHistory, useParams } from 'react-router-dom';
import { Dialog, DialogContent } from '../components/Dialog';
import { AppInfo } from '../components/AppInfo';
import { useCharge } from '../state/docket';
import { useVat } from '../state/kiln';

export const TileInfo = () => {
  const { desk } = useParams<{ desk: string }>();
  const { push } = useHistory();
  const charge = useCharge(desk);
  const vat = useVat(desk);

  if (!charge) {
    return null;
  }

  return (
    <Dialog open onOpenChange={(open) => !open && push('/')}>
      <DialogContent>
        <AppInfo vat={vat} docket={charge} />
      </DialogContent>
    </Dialog>
  );
};
