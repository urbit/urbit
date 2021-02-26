export interface GcpToken {
  accessKey: string;
  expiresIn: number;
};

export interface GcpState {
  configured?: boolean;
  token?: GcpToken
};
