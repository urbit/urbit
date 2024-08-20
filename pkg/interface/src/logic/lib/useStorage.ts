import S3Client from './S3Client';
import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import useStorageState from '../state/storage';
import GcpClient from './GcpClient';
import { StorageAcl, StorageClient } from './StorageClient';
import { dateToDa, deSig } from './util';

export interface IuseStorage {
  canUpload: boolean;
  upload: (file: File, bucket: string) => Promise<string>;
  uploadDefault: (file: File) => Promise<string>;
  uploading: boolean;
  promptUpload: (onError?: (err: Error) => void) => Promise<string>;
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
      client.current = new S3Client({
        credentials: s3.credentials,
        endpoint: s3.credentials.endpoint,
        signatureVersion: 'v4'
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
    async (file: File, bucket: string): Promise<string> => {
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

  const uploadDefault = useCallback(async (file: File): Promise<string> => {
    if (s3.configuration.currentBucket === '') {
      throw new Error('current bucket not set');
    }
    return upload(file, s3.configuration.currentBucket);
  }, [s3, upload]);

  const promptUpload = useCallback(
    (onError?: (err: Error) => void): Promise<string> => {
      return new Promise((resolve, reject) => {
        const fileSelector = document.createElement('input');
        fileSelector.setAttribute('type', 'file');
        fileSelector.setAttribute('accept', accept);
        fileSelector.style.visibility = 'hidden';
        fileSelector.addEventListener('change', () => {
          const files = fileSelector.files;
          if (!files || files.length <= 0) {
            reject();
          } else if (onError) {
            uploadDefault(files[0]).then(resolve).catch(err => onError(err));
            document.body.removeChild(fileSelector);
          } else {
            uploadDefault(files[0]).then(resolve);
            document.body.removeChild(fileSelector);
          }
        });
        document.body.appendChild(fileSelector);
        fileSelector.click();
      });
    },
    [uploadDefault]
  );

  return { canUpload, upload, uploadDefault, uploading, promptUpload };
};

export default useStorage;
