import { kilnBump, Vat } from '@urbit/api';
import { partition, pick } from 'lodash';
import { useCallback } from 'react';
import { useHistory } from 'react-router-dom';
import api from '../state/api';
import { useCharges } from '../state/docket';
import useKilnState, { useVat } from '../state/kiln';

export function vatIsBlocked(newKelvin: number | undefined, vat: Vat) {
  if (!newKelvin) {
    return false;
  }

  return !(vat.arak?.rail?.next || []).find(({ weft }) => weft.kelvin === newKelvin);
}

export function useSystemUpdate() {
  const { push } = useHistory();
  const base = useVat('base');
  const update = base?.arak?.rail?.next?.[0];
  const newKelvin = update?.weft?.kelvin;
  const charges = useCharges();
  const [blocked] = useKilnState((s) => {
    const [b, u] = partition(Object.entries(s.vats), ([, vat]) => vatIsBlocked(newKelvin, vat));
    return [b.map(([d]) => d), u.map(([d]) => d)] as const;
  });

  const systemBlocked = update && blocked;
  const blockedCharges = Object.values(pick(charges, blocked));
  const blockedCount = blockedCharges.length;

  const freezeApps = useCallback(async () => {
    api.poke(kilnBump(true));
    push('/leap/upgrading');
  }, []);

  return {
    base,
    update,
    systemBlocked,
    blockedCharges,
    blockedCount,
    freezeApps
  };
}
