export type DeskStatus = 'active' | 'suspended';

export type DocketHref = DocketHrefSite | DocketHrefGlob;

export interface DocketHrefGlob {
  glob: {
    base: string;

  }
}

export interface DocketHrefSite {
  site: string;
}

export interface Docket {
  title: string;
  info?: string;
  color: string;
  href: DocketHref;
  website: string;
  license: string;
  version: string;
  image?: string;
}

export interface Charge extends Docket {
  chad: Chad;
}

export type Chad = HungChad | GlobChad | SiteChad | InstallChad | SuspendChad;

export interface HungChad {
  hung: string;
}

export interface GlobChad {
  glob: null;
}
export interface SiteChad {
  site: null;
}
export interface InstallChad {
  install: null;

}
export interface SuspendChad {
  suspend: null;
}

export interface Treaty extends Docket {
  ship: string;
  desk: string;
  cass: string;
  hash: string;
}

export interface Charges {
  [desk: string]: Charge;
}

export interface Treaties {
  [ref: string]: Treaty;
}

export type Charter = string[];

export interface Allies {
  [ship: string]: Charter;
}

export interface Provider {
  shipName: string;
  nickname?: string;
  status?: string;
}

export type ChargeUpdate = ChargeUpdateInitial | ChargeUpdateAdd | ChargeUpdateDel;

export interface ChargeUpdateInitial {
  initial: {
    [desk: string]: Charge;
  }
}

export interface ChargeUpdateAdd {
  'add-charge': {
    desk: string;
    charge: Charge;
  }
}

export interface ChargeUpdateDel {
  'del-charge': string;
}

export type AllyUpdate = AllyUpdateIni | AllyUpdateAdd | AllyUpdateDel | AllyUpdateNew;

export interface AllyUpdateIni {
  ini: {
    [ship: string]: string[];
  }
}

export interface AllyUpdateAdd {
  add: string;
}

export interface AllyUpdateDel {
  del: string;
}

export interface AllyUpdateNew {
  new: {
    ship: string;
    alliance: string[];
  }
}

export type TreatyUpdate = TreatyUpdateIni | TreatyUpdateAdd | TreatyUpdateDel;

export interface TreatyUpdateIni {
  ini: {
    [foreignDesk: string]: Treaty;
  }
}

export interface TreatyUpdateAdd {
  add: Treaty;
}

export interface TreatyUpdateDel {
  del: string;
}
