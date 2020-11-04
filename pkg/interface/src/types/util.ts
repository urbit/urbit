
export type PropFunc<T extends (...args: any[]) => any> = Parameters<T>[0];
