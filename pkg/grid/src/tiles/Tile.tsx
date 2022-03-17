import classNames from 'classnames';
import React, { FunctionComponent } from 'react';
import { useDrag } from 'react-dnd';
import { chadIsRunning } from '@urbit/api';
import { TileMenu } from './TileMenu';
import { Spinner } from '../components/Spinner';
import { getAppHref } from '../state/util';
import { useRecentsStore } from '../nav/search/Home';
import { ChargeWithDesk } from '../state/docket';
import { useTileColor } from './useTileColor';
import { useVat } from '../state/kiln';
import { Bullet } from '../components/icons/Bullet';
import { dragTypes } from './TileGrid';

type TileProps = {
  charge: ChargeWithDesk;
  desk: string;
  disabled?: boolean;
};

export const Tile: FunctionComponent<TileProps> = ({ charge, desk, disabled = false }) => {
  const addRecentApp = useRecentsStore((state) => state.addRecentApp);
  const { title, image, color, chad, href } = charge;
  const vat = useVat(desk);
  const { lightText, tileColor, menuColor, suspendColor, suspendMenuColor } = useTileColor(color);
  const loading = !disabled && 'install' in chad;
  const suspended = disabled || 'suspend' in chad;
  const hung = 'hung' in chad;
  const active = !disabled && chadIsRunning(chad);
  const link = getAppHref(href);
  const backgroundColor = suspended ? suspendColor : active ? tileColor || 'purple' : suspendColor;

  const [{ isDragging }, drag] = useDrag(() => ({
    type: dragTypes.TILE,
    item: { desk },
    collect: (monitor) => ({
      isDragging: !!monitor.isDragging()
    })
  }));

  return (
    <a
      ref={drag}
      href={active ? link : undefined}
      target="_blank"
      rel="noreferrer"
      className={classNames(
        'group absolute font-semibold w-full h-full rounded-3xl default-ring focus-visible:ring-4 overflow-hidden',
        isDragging && 'opacity-0',
        lightText && active && !loading ? 'text-gray-200' : 'text-gray-800',
        !active && 'cursor-default'
      )}
      style={{ backgroundColor }}
      onClick={() => addRecentApp(desk)}
      onAuxClick={() => addRecentApp(desk)}
    >
      <div>
        <div className="absolute z-10 top-4 left-4 sm:top-6 sm:left-6 flex items-center">
          {!active && (
            <>
              {loading && <Spinner className="h-6 w-6 mr-2" />}
              <span className="text-gray-500">
                {suspended ? 'Suspended' : loading ? 'Installing' : hung ? 'Errored' : null}
              </span>
            </>
          )}
        </div>
        {vat?.arak.rail?.paused && !disabled && (
          <Bullet className="absolute z-10 top-5 left-5 sm:top-7 sm:left-7 w-4 h-4 text-orange-500 dark:text-black" />
        )}
        <TileMenu
          desk={desk}
          chad={chad}
          menuColor={active ? menuColor : suspendMenuColor}
          lightText={lightText}
          className="absolute z-10 top-3 right-3 sm:top-5 sm:right-5 opacity-0 pointer-coarse:opacity-100 hover-none:opacity-100 focus:opacity-100 group-hover:opacity-100"
        />
        {title && (
          <div
            className="h4 absolute z-10 bottom-[8%] left-[5%] sm:bottom-7 sm:left-5 py-1 px-3 rounded-lg"
            style={{ backgroundColor }}
          >
            <h3 className="mix-blend-hard-light">{title}</h3>
          </div>
        )}
        {image && !loading && (
          <img className="absolute top-0 left-0 h-full w-full object-cover" src={image} alt="" />
        )}
      </div>
    </a>
  );
};
