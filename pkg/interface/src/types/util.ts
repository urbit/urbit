import { Icon } from "@tlon/indigo-react";
export type PropFunc<T extends (...args: any[]) => any> = Parameters<T>[0];

export type IconRef = PropFunc<typeof Icon>['icon'];
