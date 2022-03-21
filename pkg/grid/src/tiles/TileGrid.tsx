import React, { useEffect } from 'react';
import { DndProvider } from 'react-dnd';
import { HTML5Backend } from 'react-dnd-html5-backend';
import { TouchBackend } from 'react-dnd-touch-backend';
import { uniq } from 'lodash';
import { ChargeWithDesk, useCharges } from '../state/docket';
import { Tile } from './Tile';
import { MenuState } from '../nav/Nav';
import { SettingsState, useSettingsState } from '../state/settings';
import { TileContainer } from './TileContainer';
import { useMedia } from '../logic/useMedia';

export interface TileData {
  desk: string;
  charge: ChargeWithDesk;
  position: number;
  dragging: boolean;
}

interface TileGridProps {
  menu?: MenuState;
}

export const dragTypes = {
  TILE: 'tile'
};

export const selTiles = (s: SettingsState) => ({
  order: s.tiles.order,
  loaded: s.loaded
});

export const TileGrid = ({ menu }: TileGridProps) => {
  const charges = useCharges();
  const chargesLoaded = Object.keys(charges).length > 0;
  const { order, loaded } = useSettingsState(selTiles);
  const isMobile = useMedia('(pointer: coarse)');

  useEffect(() => {
    const hasKeys = order && !!order.length;
    const chargeKeys = Object.keys(charges);
    const hasChargeKeys = chargeKeys.length > 0;

    if (!loaded) {
      return;
    }

    // Correct order state, fill if none, remove duplicates, and remove
    // old uninstalled app keys
    if (!hasKeys && hasChargeKeys) {
      useSettingsState.getState().putEntry('tiles', 'order', chargeKeys);
    } else if (order.length < chargeKeys.length) {
      useSettingsState.getState().putEntry('tiles', 'order', uniq(order.concat(chargeKeys)));
    } else if (order.length > chargeKeys.length && hasChargeKeys) {
      useSettingsState
        .getState()
        .putEntry('tiles', 'order', uniq(order.filter((key) => key in charges).concat(chargeKeys)));
    }
  }, [charges, order, loaded]);

  if (!chargesLoaded) {
    return <span>Loading...</span>;
  }

  return (
    <DndProvider
      backend={isMobile ? TouchBackend : HTML5Backend}
      options={
        isMobile
          ? {
              delay: 50,
              scrollAngleRanges: [
                { start: 30, end: 150 },
                { start: 210, end: 330 }
              ]
            }
          : undefined
      }
    >
      <div className="grid justify-center grid-cols-2 sm:grid-cols-[repeat(auto-fit,minmax(auto,250px))] gap-4 px-4 md:px-8 w-full max-w-6xl">
        {order
          .filter((d) => d !== window.desk && d in charges)
          .map((desk) => (
            <TileContainer key={desk} desk={desk}>
              <Tile charge={charges[desk]} desk={desk} disabled={menu === 'upgrading'} />
            </TileContainer>
          ))}
      </div>
    </DndProvider>
  );
};
