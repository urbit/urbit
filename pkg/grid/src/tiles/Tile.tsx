import classNames from 'classnames';
import React, { FunctionComponent } from 'react';
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

export const Tile: FunctionComponent<TileProps> = ({ charge, desk }) => {
  const addRecentApp = useRecentsStore((state) => state.addRecentApp);
  const { title, image, color, chad, href } = charge;
  const { lightText, tileColor, menuColor, suspendColor, suspendMenuColor } = useTileColor(color);
  const loading = 'install' in chad;
  const suspended = 'suspend' in chad;
  const active = chadIsRunning(chad);
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
          <div className="absolute z-10 top-4 left-4 sm:top-8 sm:left-8 flex items-center justify-center">
            <Spinner className="h-6 w-6" />
          </div>
        ) : (
          <TileMenu
            desk={desk}
            active={active}
            menuColor={active ? menuColor : suspendMenuColor}
            lightText={lightText}
            className="absolute z-10 top-2.5 right-2.5 sm:top-4 sm:right-4 opacity-0 pointer-coarse:opacity-100 hover-none:opacity-100 focus:opacity-100 group-hover:opacity-100"
          />
        )}
        <div
          className="h4 absolute z-10 bottom-[8%] left-[5%] sm:bottom-7 sm:left-5 py-1 px-3 rounded-lg"
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
