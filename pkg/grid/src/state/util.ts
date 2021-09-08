import { DocketHref } from '@urbit/api/docket';

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
