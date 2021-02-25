import {useCallback, useMemo, useEffect, useRef, useState} from 'react';
import {S3State} from '../../types/s3-update';
import {GcpState} from '../../types/gcp-state';
import GcpClient from './GcpClient';
import {dateToDa, deSig} from "./util";


// TODO: unify with IuseS3
export interface IuseGcp {
  canUpload: boolean;
  upload: (file: File, bucket: string) => Promise<string>;
  uploadDefault: (file: File) => Promise<string>;
  uploading: boolean;
  promptUpload: () => Promise<string | undefined>;
}

// TODO: unify with useS3 / expose useStorage
const useGcp = (s3: S3State, gcp: GcpState, { accept = '*' } = { accept: '*' }): IuseGcp => {
  const [uploading, setUploading] = useState(false);

  const client = useRef<GcpClient | null>(null);

  useEffect(() => {
    if (!gcp.accessKey) {
      return;
    }
    client.current = new GcpClient(gcp.accessKey);
  }, [gcp.accessKey]);

  const canUpload = useMemo(
    () =>
      (client && gcp.accessKey && s3.configuration.currentBucket !== "") || false,
    [gcp.accessKey, s3.configuration.currentBucket, client]
  );

  const upload = useCallback(
    async (file: File, bucket: string) => {
      if (!client.current) {
        throw new Error("Gcp not ready");
      }

      const fileParts = file.name.split('.');
      const fileName = fileParts.slice(0, -1);
      const fileExtension = fileParts.pop();
      const timestamp = deSig(dateToDa(new Date()));

      const params = {
        bucket,
        name: `${window.ship}/${timestamp}-${fileName}.${fileExtension}`,
        body: file,
        predefinedAcl: 'publicRead',
        type: file.type,
      };

      setUploading(true);

      const location = await client.current.upload(params);

      setUploading(false);

      return location;
    },
    [client, setUploading]
  );

  const uploadDefault = useCallback(async (file: File) => {
    if (s3.configuration.currentBucket == '') {
      throw new Error('current bucket not set');
    }
    return upload(file, s3.configuration.currentBucket);
  }, [s3]);

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

export default useGcp;
