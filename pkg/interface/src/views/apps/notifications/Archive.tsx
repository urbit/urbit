import { Box } from '@tlon/indigo-react';
import React, { useEffect } from 'react';
import useHarkState, { HarkState } from '~/logic/state/hark';
import { Notification } from './notification';

const selArchive = (s: HarkState) => s.archive;

export function Archive() {
  const archive = useHarkState(selArchive);
  const keys = archive.keys();

  useEffect(() => {
    useHarkState.getState().getMore();
  }, []);

  return (
    <Box pt="2" overflowY="auto" overflowX="hidden">
      {keys.map(key =>
        Object.entries(archive.get(key)!)
          .sort(([, a], [, b]) => b.time - a.time)
          .map(([binId, n]) => (
            <Notification
              key={`${key.toString()}-${binId}`}
              lid={{ time: key.toString() }}
              notification={n}
            />
          ))
      )}
    </Box>
  );
}
