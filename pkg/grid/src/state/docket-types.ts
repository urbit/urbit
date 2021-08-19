export type DeskStatus = 'active' | 'suspended';

export interface Docket {
  title: string;
  info?: string;
  color: string;
  base: string;
  website: string;
  license: string;
  version: string;

  // yet to be implemented
  img?: string;
  status: DeskStatus;
}

export interface Treaty extends Docket {
  ship: string;
  desk: string;
  cass: string;
  hash: string;
}

export interface Provider {
  shipName: string;
  nickname?: string;
  status?: string;
  treaties?: string[];
}

export type Dockets = Record<string, Docket>;

export type Treaties = Record<string, Treaty>;

export type Providers = Record<string, Provider>;
