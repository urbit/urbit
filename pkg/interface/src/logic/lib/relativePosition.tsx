import _ from 'lodash';

export const alignY = ['top', 'bottom'] as const;
export type AlignY = typeof alignY[number];
export const alignX = ['left', 'right'] as const;
export type AlignX = typeof alignX[number];

export function getRelativePosition(
  relativeTo: HTMLElement | null,
  alignX: AlignX | AlignX[],
  alignY: AlignY | AlignY[],
  offsetX = 0,
  offsetY = 0
) {
  const rect = relativeTo?.getBoundingClientRect();
  if (!rect) {
    return {};
  }
  const bounds = {
    top: rect.top - offsetY,
    left: rect.left - offsetX,
    bottom: document.documentElement.clientHeight - rect.bottom - offsetY,
    right: document.documentElement.clientWidth - rect.right - offsetX
  };
  const alignXArr = _.isArray(alignX) ? alignX : [alignX];
  const alignYArr = _.isArray(alignY) ? alignY : [alignY];

  return {
    ..._.reduce(
      alignXArr,
      (acc, a, idx) => ({
        ...acc,
        [a]: _.zipWith(
          [...Array(idx), `${bounds[a]}px`],
          acc[a] || [],
          (a, b) => a || b || null
        )
      }),
      {}
    ),
    ..._.reduce(
      alignYArr,
      (acc, a, idx) => ({
        ...acc,
        [a]: _.zipWith(
          [...Array(idx), `${bounds[a]}px`],
          acc[a] || [],
          (a, b) => a || b || null
        )
      }),
      {}
    )
  } as Record<AlignY | AlignX, string[]>;
}

