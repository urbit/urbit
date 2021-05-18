export interface GcpToken {
  accessKey: string;
  expiresIn: number;
}

export interface GcpState {
  token?: GcpToken
}
