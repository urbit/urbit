import { Action, Mark, Poke } from '../../../api/index';

export interface PokeInterface extends Poke<Mark, any> {
  onSuccess?: () => void;
  onError?: () => void;
}

export interface AuthenticationInterface {
  ship: string;
  url: string;
  code: string;
  verbose?: boolean;
}

export interface SubscriptionInterface {
  err?(error: any): void;
  event?(data: any): void;
  quit?(data: any): void;
}

export type SubscriptionRequestInterface = SubscriptionInterface & {
  app: string;
  path: string;
}

export interface headers {
  'Content-Type': string;
  Cookie?: string;
}

export interface UrbitInterface {
  connect(): void;
}

export interface CustomEventHandler {
  (data: any, response: string): void;
}

export interface SSEOptions {
  headers?: {
    Cookie?: string
  };
  withCredentials?: boolean;
}