import classNames from 'classnames';
import { uniq, without } from 'lodash';
import React, { FunctionComponent } from 'react';
import { useDrop } from 'react-dnd';
import { useSettingsState } from '../state/settings';
import { dragTypes, selTiles } from './TileGrid';

interface TileContainerProps {
  desk: string;
}

export const TileContainer: FunctionComponent<TileContainerProps> = ({ desk, children }) => {
  const { order } = useSettingsState(selTiles);
  const [{ isOver }, drop] = useDrop<{ desk: string }, undefined, { isOver: boolean }>(
    () => ({
      accept: dragTypes.TILE,
      drop: ({ desk: itemDesk }) => {
        if (!itemDesk || itemDesk === desk) {
          return undefined;
        }
        // [1, 2, 3, 4] 1 -> 3
        // [2, 3, 4]
        const beforeSlot = order.indexOf(itemDesk) < order.indexOf(desk);
        const orderWithoutOriginal = without(order, itemDesk);
        const slicePoint = orderWithoutOriginal.indexOf(desk);
        // [2, 3] [4]
        const left = orderWithoutOriginal.slice(0, beforeSlot ? slicePoint + 1 : slicePoint);
        const right = orderWithoutOriginal.slice(slicePoint);
        // concat([2, 3], [1], [4])
        const newOrder = uniq(left.concat([itemDesk], right));
        // [2, 3, 1, 4]
        console.log({ order, left, right, slicePoint, newOrder });
        useSettingsState.getState().putEntry('tiles', 'order', newOrder);

        return undefined;
      },
      collect: (monitor) => ({
        isOver: !!monitor.isOver()
      })
    }),
    [desk, order]
  );

  return (
    <div
      ref={drop}
      className={classNames(
        'relative aspect-w-1 aspect-h-1 rounded-3xl ring-4',
        isOver && 'ring-blue-500',
        !isOver && 'ring-transparent'
      )}
    >
      {children}
    </div>
  );
};
