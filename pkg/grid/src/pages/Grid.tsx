import { map, omit } from 'lodash';
import React, { FunctionComponent } from 'react';
import { Route, RouteComponentProps } from 'react-router-dom';
import { MenuState, Nav } from '../nav/Nav';
import { useCharges } from '../state/docket';
import { RemoveApp } from '../tiles/RemoveApp';
import { SuspendApp } from '../tiles/SuspendApp';
import { Tile } from '../tiles/Tile';
import { TileInfo } from '../tiles/TileInfo';

type GridProps = RouteComponentProps<{
  menu?: MenuState;
}>;

export const Grid: FunctionComponent<GridProps> = ({ match }) => {
  const charges = useCharges();
  const chargesLoaded = Object.keys(charges).length > 0;

  return (
    <div className="flex flex-col">
      <header className="fixed sm:sticky bottom-0 sm:bottom-auto sm:top-0 left-0 z-30 flex justify-center w-full px-4 bg-white">
        <Nav menu={match.params.menu} />
      </header>

      <main className="h-full w-full flex justify-center pt-4 md:pt-16 pb-32 relative z-0">
        {!chargesLoaded && <span>Loading...</span>}
        {chargesLoaded && (
          <div className="grid justify-center grid-cols-2 sm:grid-cols-[repeat(auto-fit,minmax(auto,250px))] gap-4 px-4 md:px-8 w-full max-w-6xl">
            {charges &&
              map(omit(charges, window.desk), (charge, desk) => (
                <Tile key={desk} charge={charge} desk={desk} />
              ))}
          </div>
        )}
        <Route exact path="/app/:desk">
          <TileInfo />
        </Route>
        <Route exact path="/app/:desk/suspend">
          <SuspendApp />
        </Route>
        <Route exact path="/app/:desk/remove">
          <RemoveApp />
        </Route>
      </main>
    </div>
  );
};
