import clipboardCopy from 'clipboard-copy';
import { useCallback } from 'react';
import { useMutation, useQuery } from 'react-query';
import { useParams } from 'react-router-dom';
import { installDocket, requestTreaty, treatyKey } from '../state/docket';

export function useTreaty() {
  const { ship, desk } = useParams<{ ship: string; desk: string }>();
  const { data: treaty } = useQuery(treatyKey([ship, desk]), () => requestTreaty(ship, desk));
  const { mutate, ...installStatus } = useMutation(() => installDocket(ship, desk));

  const copyApp = useCallback(async () => {
    clipboardCopy(`${ship}/${desk}`);
  }, [ship, desk]);

  const installApp = useCallback(() => {
    mutate();
  }, []);

  return {
    ship,
    desk,
    treaty,
    installStatus,
    installApp,
    copyApp
  };
}
