import clipboardCopy from 'clipboard-copy';
import { pick } from 'lodash-es';
import { useCallback, useEffect, useState } from 'react';
import { useParams } from 'react-router-dom';
import useDocketState from '../state/docket';
import { Treaty } from '../state/docket-types';

type Status = 'initial' | 'loading' | 'success' | 'error';

export function useTreaty() {
  const { ship, desk } = useParams<{ ship: string; desk: string }>();
  const { requestTreaty, installDocket } = useDocketState((s) =>
    pick(s, ['requestTreaty', 'installDocket'])
  );
  const [treaty, setTreaty] = useState<Treaty>();
  const [installStatus, setInstallStatus] = useState<Status>('initial');

  useEffect(() => {
    async function getTreaty() {
      setTreaty(await requestTreaty(ship, desk));
    }

    getTreaty();
  }, [ship, desk]);

  const copyApp = useCallback(async () => {
    clipboardCopy(`${ship}/${desk}`);
  }, [ship, desk]);

  const installApp = useCallback(async () => {
    setInstallStatus('loading');

    installDocket(ship, desk)
      .then(() => setInstallStatus('success'))
      .catch(() => setInstallStatus('error'));
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
