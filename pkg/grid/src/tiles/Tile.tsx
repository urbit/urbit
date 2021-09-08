import classNames from 'classnames';
import React, { FunctionComponent } from 'react';
import { darken, hsla, lighten, parseToHsla, readableColorIsBlack } from 'color2k';
import { chadIsRunning } from '@urbit/api/docket';
import { TileMenu } from './TileMenu';
import { Spinner } from '../components/Spinner';
import { getAppHref } from '../state/util';
import { useRecentsStore } from '../nav/search/Home';
import { ChargeWithDesk } from '../state/docket';
import { useTileColor } from './useTileColor';

type TileProps = {
  charge: ChargeWithDesk;
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

export const Tile: FunctionComponent<TileProps> = ({ charge, desk }) => {
  const addRecentApp = useRecentsStore((state) => state.addRecentApp);
  const { title, image, color, chad, href } = charge;
  const { theme, tileColor } = useTileColor(color);
  const loading = 'install' in chad;
  const active = chadIsRunning(chad);
  const lightText = !readableColorIsBlack(color);
  const menuColor = getMenuColor(tileColor, theme === 'dark' ? !lightText : lightText, active);
  const suspendColor = 'rgb(220,220,220)';
  const suspended = 'suspend' in chad;
  const link = getAppHref(href);
  const backgroundColor = active ? tileColor || 'purple' : suspendColor;

  return (
    <a
      href={active ? link : undefined}
      target={desk}
      className={classNames(
        'group relative font-semibold aspect-w-1 aspect-h-1 rounded-3xl default-ring focus-visible:ring-4 overflow-hidden',
        lightText && active && !loading ? 'text-gray-200' : 'text-gray-800',
        !active && 'cursor-default'
      )}
      style={{ backgroundColor }}
      onClick={() => addRecentApp(charge)}
      onAuxClick={() => addRecentApp(charge)}
    >
      <div>
        {loading ? (
          <div className="absolute z-10 top-4 left-4 lg:top-8 lg:left-8 flex items-center justify-center">
            <Spinner className="h-6 w-6" />
          </div>
        ) : (
          <TileMenu
            desk={desk}
            active={active}
            menuColor={menuColor}
            lightText={lightText}
            className="absolute z-10 top-2.5 right-2.5 sm:top-4 sm:right-4 opacity-0 hover-none:opacity-100 focus:opacity-100 group-hover:opacity-100"
          />
        )}
        <div
          className="h4 absolute z-10 bottom-3 left-1 lg:bottom-7 lg:left-5 py-1 px-3 rounded-lg"
          style={{ backgroundColor }}
        >
          <h3 className="mix-blend-hard-light">{title}</h3>
          {!active && (
            <span className="text-gray-400">{suspended ? 'Suspended' : 'Installing'}</span>
          )}
        </div>
        {image && !loading && (
          <img className="absolute top-0 left-0 h-full w-full object-contain" src={image} alt="" />
        )}
      </div>
    </a>
  );
};
