
export type LaunchUpdate =
  LaunchUpdateInitial
| LaunchUpdateFirstTime
| LaunchUpdateOrder
| LaunchUpdateIsShown;


interface LaunchUpdateInitial {
  initial: LaunchState;
}

interface LaunchUpdateFirstTime {
  changeFirstTime: boolean;
}

interface LaunchUpdateOrder {
  changeOrder: string[];
}

interface LaunchUpdateIsShown {
  changeIsShown: {
    name: string;
    isShown: boolean;
  }
}

export interface LaunchState {
  firstTime: boolean;
  tileOrdering: string[];
  tiles: {
    [app: string]: Tile;
  }
}

interface Tile {
  isShown: boolean;
  type: TileType;
}

type TileType = TileTypeBasic | TileTypeCustom;

interface TileTypeBasic {
  basic: {
    iconUrl: string;
    linkedUrl: string;
    title: string;
  }
}

interface TileTypeCustom {
  custom: null;
}
