import { DocketHref } from '@urbit/api/docket';

export function makeKeyFn(key: string) {
  return (childKeys: string[] = []) => {
    return [key].concat(childKeys);
  };
}

export const useMockData = import.meta.env.MODE === 'mock';

export async function fakeRequest<T>(data?: any, time = 300): Promise<T> {
  return new Promise((resolve) => {
    setTimeout(() => {
      resolve(data);
    }, time);
  });
}

export function getAppHref(href: DocketHref) {
  return 'site' in href ? href.site : `/apps/${href.glob.base}`;
}

export function disableDefault<T extends Event>(e: T): void {
  e.preventDefault();
}
