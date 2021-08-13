import classNames from 'classnames';
import React, { FunctionComponent } from 'react';
import { darken, hsla, lighten, parseToHsla, readableColorIsBlack } from 'color2k';
import { TileMenu } from './TileMenu';
import { Docket } from '../state/docket-types';

type TileProps = {
  docket: Docket;
  desk: string;
};

function getMenuColor(color: string, lightText: boolean, active: boolean): string {
  const hslaColor = parseToHsla(color);
  const satAdjustedColor = hsla(
    hslaColor[0],
    active ? Math.max(0.2, hslaColor[1]) : 0,
    hslaColor[2],
    1
  );

  return lightText ? lighten(satAdjustedColor, 0.1) : darken(satAdjustedColor, 0.1);
}

export const Tile: FunctionComponent<TileProps> = ({ docket, desk }) => {
  const { title, base, color, img, status } = docket;
  const active = status === 'active';
  const lightText = !readableColorIsBlack(color);
  const menuColor = getMenuColor(color, lightText, active);
  const suspendColor = 'rgb(220,220,220)';

  return (
    <a
      href={active ? `/apps/${base}/` : undefined}
      target={base}
      className={classNames(
        'group relative font-semibold aspect-w-1 aspect-h-1 rounded-xl default-ring',
        !active && 'cursor-default'
      )}
      style={{ backgroundColor: active ? color || 'purple' : suspendColor }}
    >
      <div>
        <TileMenu
          desk={desk}
          active={active}
          menuColor={menuColor}
          lightText={lightText}
          className="absolute z-10 top-2.5 right-2.5 sm:top-4 sm:right-4 opacity-0 hover-none:opacity-100 focus:opacity-100 group-hover:opacity-100"
        />
        <div className="h4 absolute bottom-4 left-4 lg:bottom-8 lg:left-8">
          <h3
            className={`${
              lightText && active ? 'text-gray-200' : 'text-gray-800'
            }  mix-blend-hard-light`}
          >
            {title}
          </h3>
          {!active && <span className="text-gray-400">Suspended</span>}
        </div>
        {img && (
          <img
            className="absolute top-1/2 left-1/2 h-[40%] w-[40%] object-contain transform -translate-x-1/2 -translate-y-1/2"
            src={img}
            alt=""
          />
        )}
      </div>
    </a>
  );
};
