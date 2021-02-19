import { joinError, joinProgress, joinResult } from ".";

export type JoinError = typeof joinError[number];

export type JoinResult = typeof joinResult[number];


export type JoinProgress = typeof joinProgress[number];

export interface JoinRequests {
  [rid: string]: JoinProgress;
}
