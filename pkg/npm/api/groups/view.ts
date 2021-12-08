import { joinError, joinProgress, joinResult } from ".";
import {Patp} from "../lib";

export type JoinError = typeof joinError[number];

export type JoinResult = typeof joinResult[number];


export type JoinProgress = typeof joinProgress[number];

export interface JoinRequest {
  /**
   *  Whether to display the join request or not
   */
  hidden: boolean;
  /**
   *  Timestamp of when the request started
   */
  started: number;
  ship: Patp;
  progress: JoinProgress;
  shareContact: boolean;
  autojoin: boolean;
  app: 'graph' | 'groups';
  invite: string[];
}

export interface JoinRequests {
  [rid: string]: JoinRequest;
}
