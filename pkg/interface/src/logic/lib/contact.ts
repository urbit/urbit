import airlock from '~/logic/api';
import _ from 'lodash';

export async function disallowedShipsForOurContact(
  ships: string[]
): Promise<string[]> {
  return _.compact(
    await Promise.all(
      ships.map(async (s) => {
        const ship = `~${s}`;
        if (s === window.ship) {
          return null;
        }
        const allowed = await airlock.scry(fetchIsAllowed(
          `~${window.ship}`,
          'personal',
          ship,
          true
        ));
        return allowed ? null : ship;
      })
    )
  );
}
