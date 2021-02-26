import {useCallback, useMemo, useEffect, useRef, useState} from 'react';
import {S3State} from '../../types/s3-update';
import {GcpState} from '../../types/gcp-state';
import S3 from "aws-sdk/clients/s3";
import GcpClient from './GcpClient';
import {StorageClient, StorageAcl} from './StorageClient';
import {dateToDa, deSig} from "./util";


export interface IuseStorage {
  canUpload: boolean;
  upload: (file: File, bucket: string) => Promise<string>;
  uploadDefault: (file: File) => Promise<string>;
  uploading: boolean;
  promptUpload: () => Promise<string | undefined>;
}

const useStorage = (s3: S3State, gcp: GcpState,
                    { accept = '*' } = { accept: '*' }): IuseStorage => {
  const [uploading, setUploading] = useState(false);

  const gcpClient = useRef<GcpClient | null>(null);
  const s3Client = useRef<S3Client | null>(null);

  useEffect(() => {
    // prefer GCP if available, else use S3.
    if (gcp.accessKey) {
      gcpClient.current = new GcpClient(gcp.accessKey);
      s3Client.current = null;
    } else {
      if (!s3.credentials) {
        return;
      }
      s3Client.current = new S3({
        credentials: s3.credentials,
        endpoint: s3.credentials.endpoint
      });
      gcpClient.current = null;
    }
  }, [gcp.accessKey, s3.credentials]);

  const canUpload = useMemo(
    () =>
      ((gcpClient || s3Client) && s3.configuration.currentBucket !== "") || false,
    [gcpClient, s3Client, s3.configuration.currentBucket]
  );

  const upload = useCallback(
    async (file: File, bucket: string) => {
      const client: StorageClient | null =
        gcpClient.current || s3Client.current;
      if (!client) {
        throw new Error("Storage not ready");
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
        ContentType: file.type,
      };

      setUploading(true);

      const { Location } = await client.upload(params);

      setUploading(false);

      return Location;
    },
    [gcpClient, s3Client, setUploading]
  );

  const uploadDefault = useCallback(async (file: File) => {
    if (s3.configuration.currentBucket == '') {
      throw new Error('current bucket not set');
    }
    return upload(file, s3.configuration.currentBucket);
  }, [s3.configuration, upload]);

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
        })
        document.body.appendChild(fileSelector);
        fileSelector.click();
      })
    },
    [uploadDefault]
  );

  return {canUpload, upload, uploadDefault, uploading, promptUpload};
};

export default useStorage;
