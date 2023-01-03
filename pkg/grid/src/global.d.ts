declare module 'urbit-ob' {
  export function isValidPatp(patp: string): boolean;
}

type Stringified<T> = string &
  {
    [P in keyof T]: { '_ value': T[P] };
  };

interface JSON {
  // stringify(value: any, replacer?: (key: string, value: any) => any, space?: string | number): string;
  stringify<T>(
    value: T,
    replacer?: (key: string, value: any) => any,
    space?: string | number
  ): string & Stringified<T>;
  // parse(text: string, reviver?: (key: any, value: any) => any): any;
  parse<T>(text: Stringified<T>, reviver?: (key: any, value: any) => any): T;
}
