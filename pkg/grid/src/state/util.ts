import { Docket, DocketHref, Treaty } from '@urbit/api';
import { hsla, parseToHsla } from 'color2k';
import _ from 'lodash';

export const useMockData = import.meta.env.MODE === 'mock';

export async function fakeRequest<T>(data: T, time = 300): Promise<T> {
  return new Promise((resolve) => {
    setTimeout(() => {
      resolve(data);
    }, time);
  });
}

export function getAppHref(href: DocketHref) {
  return 'site' in href ? href.site : `/apps/${href.glob.base}/`;
}

export function getAppName(app: (Docket & { desk: string }) | Treaty | undefined): string {
  if (!app) {
    return '';
  }

  return app.title || app.desk;
}

export function disableDefault<T extends Event>(e: T): void {
  e.preventDefault();
}

// hack until radix-ui fixes this behavior
export function handleDropdownLink(setOpen?: (open: boolean) => void): (e: Event) => void {
  return (e: Event) => {
    e.stopPropagation();
    e.preventDefault();
    setTimeout(() => setOpen?.(false), 15);
  };
}

export function deSig(ship: string): string {
  if (!ship) {
    return '';
  }
  return ship.replace('~', '');
}

export function normalizeUrbitColor(color: string): string {
  if (color.startsWith('#')) {
    return color;
  }

  const colorString = color.slice(2).replace('.', '').toUpperCase();
  const lengthAdjustedColor = _.padEnd(colorString, 6, _.last(colorString));
  return `#${lengthAdjustedColor}`;
}

export function getDarkColor(color: string): string {
  const hslaColor = parseToHsla(color);
  return hsla(hslaColor[0], hslaColor[1], 1 - hslaColor[2], 1);
}

export function pluralize(word: string, count: number): string {
  if (count === 1) {
    return word;
  }

  return `${word}s`;
}

export function createStorageKey(name: string): string {
  return `~${window.ship}/${window.desk}/${name}`;
}

// for purging storage with version updates
export function clearStorageMigration<T>() {
  return {} as T;
}

export const storageVersion = parseInt(import.meta.env.VITE_STORAGE_VERSION, 10);
