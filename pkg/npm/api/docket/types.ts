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

export interface Dockets {
  [ref: string]: Docket;
}

export interface Treaties {
  [ref: string]: Treaty;
}

export interface Provider {
  shipName: string;
  nickname?: string;
  status?: string;
}

export type DocketUpdate = DocketUpdateInitial | DocketUpdateAdd | DocketUpdateDel;

export interface DocketUpdateInitial {
  initial: {
    [desk: string]: Docket;
  }
}

export interface DocketUpdateAdd {
  'add-dock': {
    desk: string;
    docket: Docket;
  }
}

export interface DocketUpdateDel {
  'del-dock': string;
}
