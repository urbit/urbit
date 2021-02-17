export const joinError = ['no-perms', 'strange'] as const;
export type JoinError = typeof joinError[number];
export const joinResult = ['done', ...joinError] as const;
export type JoinResult = typeof joinResult[number];

export const joinProgress = ['start', 'added', ...joinResult] as const;
export type JoinProgress = typeof joinProgress[number];

export interface JoinRequests {
  [rid: string]: JoinProgress;
}
