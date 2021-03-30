import { useCallback, useMemo, useEffect, useRef, useState } from 'react';
import {
  GcpState,
  S3State,
  StorageState
} from '../../types';
import S3 from 'aws-sdk/clients/s3';
import GcpClient from './GcpClient';
import { StorageClient, StorageAcl } from './StorageClient';
import { dateToDa, deSig } from './util';
import useStorageState from '../state/storage';


export interface IuseStorage {
  canUpload: boolean;
  upload: (file: File, bucket: string) => Promise<string>;
  uploadDefault: (file: File) => Promise<string>;
  uploading: boolean;
  promptUpload: () => Promise<string | undefined>;
}

const useStorage = ({ accept = '*' } = { accept: '*' }): IuseStorage => {
  const [uploading, setUploading] = useState(false);
  const gcp = useStorageState(state => state.gcp);
  const s3 = useStorageState(state => state.s3);

  const client = useRef<StorageClient | null>(null);

  useEffect(() => {
    // prefer GCP if available, else use S3.
    if (gcp.token !== undefined) {
      client.current = new GcpClient(gcp.token.accessKey);
    } else {
      // XX ships currently always have S3 credentials, but the fields are all
      // set to '' if they are not configured.
      if (!s3.credentials ||
          !s3.credentials.accessKeyId ||
          !s3.credentials.secretAccessKey) {
        return;
      }
      client.current = new S3({
        credentials: s3.credentials,
        endpoint: s3.credentials.endpoint
      });
    }
  }, [gcp.token, s3.credentials]);

  const canUpload = useMemo(
    () =>
      ((gcp.token || (s3.credentials && s3.credentials.accessKeyId &&
                      s3.credentials.secretAccessKey)) &&
       s3.configuration.currentBucket !== '') || false,
    [s3.credentials, gcp.token, s3.configuration.currentBucket]
  );

  const upload = useCallback(
    async (file: File, bucket: string) => {
      if (client.current === null) {
        throw new Error('Storage not ready');
      }

      const fileParts = file.name.split('.');
      const fileName = fileParts.slice(0, -1);
      const fileExtension = fileParts.pop();
      const timestamp = deSig(dateToDa(new Date()));

      const params = {
        Bucket: bucket,
        Key: `${window.ship}/${timestamp}-${fileName}.${fileExtension}`,
        Body: file,
        ACL: StorageAcl.PublicRead,
        ContentType: file.type
      };

      setUploading(true);

      const { Location } = await client.current.upload(params).promise();

      setUploading(false);

      return Location;
    },
    [client, setUploading]
  );

  const uploadDefault = useCallback(async (file: File) => {
    if (s3.configuration.currentBucket === '') {
      throw new Error('current bucket not set');
    }
    return upload(file, s3.configuration.currentBucket);
  }, [s3, upload]);

  const promptUpload = useCallback(
    () => {
      return new Promise((resolve, reject) => {
        const fileSelector = document.createElement('input');
        fileSelector.setAttribute('type', 'file');
        fileSelector.setAttribute('accept', accept);
        fileSelector.style.visibility = 'hidden';
        fileSelector.addEventListener('change', () => {
          const files = fileSelector.files;
          if (!files || files.length <= 0) {
            reject();
            return;
          }
          uploadDefault(files[0]).then(resolve);
          document.body.removeChild(fileSelector);
        });
        document.body.appendChild(fileSelector);
        fileSelector.click();
      });
    },
    [uploadDefault]
  );

  return {canUpload, upload, uploadDefault, uploading, promptUpload};
};

export default useStorage;
