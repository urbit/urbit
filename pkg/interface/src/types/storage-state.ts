import { GcpState } from './gcp-state';
import { S3State } from './s3-update';

export interface StorageState {
  gcp: GcpState;
  s3: S3State;
}
