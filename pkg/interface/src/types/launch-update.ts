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

export interface Tile {
  isShown: boolean;
  type: TileType;
}

type TileType = TileTypeBasic | TileTypeCustom;

export interface TileTypeBasic {
  basic: {
    iconUrl: string;
    linkedUrl: string;
    title: string;
  }
}

interface TileTypeCustom {
  custom: null;
}

interface WeatherDay {
  apparentTemperature: number;
  cloudCover: number;
  dewPoint: number;
  humidity: number;
  icon: string;
  ozone: number;
  precipIntensity: number;
  precipProbability: number;
  precipType: string;
  pressure: number;
  summary: string;
  temperature: number;
  time: number;
  uvIndex: number;
  visibility: number;
  windBearing: number;
  windGust: number;
  windSpeed: number;
}

export interface WeatherState {
  currently: WeatherDay;
  daily: {
    data: WeatherDay[];
    icon: string;
    summary: string;
  }
}
