import { useCallback, useMemo, useEffect, useRef, useState } from "react";
import { S3State } from "../../types/s3-update";
import S3 from "aws-sdk/clients/s3";
import { dateToDa, deSig } from "./util";

export interface IuseS3 {
  canUpload: boolean;
  upload: (file: File, bucket: string) => Promise<string>;
  uploadDefault: (file: File) => Promise<string>;
  uploading: boolean;
  promptUpload: () => Promise<string | undefined>;
}

const useS3 = (s3: S3State, { accept = '*' } = { accept: '*' }): IuseS3 => {
  const [uploading, setUploading] = useState(false);

  const client = useRef<S3 | null>(null);

  useEffect(() => {
    if (!s3.credentials) {
      return;
    }
    client.current = new S3({
      credentials: s3.credentials,
      endpoint: s3.credentials.endpoint
    });
  }, [s3.credentials]);

  const canUpload = useMemo(
    () =>
      (client && s3.credentials && s3.configuration.currentBucket !== "") || false,
    [s3.credentials, s3.configuration.currentBucket, client]
  );

  const upload = useCallback(
    async (file: File, bucket: string) => {
      if (!client.current) {
        throw new Error("S3 not ready");
      }

      const fileParts = file.name.split('.');
      const fileName = fileParts.slice(0, -1);
      const fileExtension = fileParts.pop();
      const timestamp = deSig(dateToDa(new Date()));

      const params = {
        Bucket: bucket,
        Key: `${window.ship}/${timestamp}-${fileName}.${fileExtension}`,
        Body: file,
        ACL: "public-read",
        ContentType: file.type,
      };

      setUploading(true);

      const { Location } = await client.current.upload(params).promise();

      setUploading(false);

      return Location;
    },
    [client, setUploading]
  );

  const uploadDefault = useCallback(async (file: File) => {
    if (s3.configuration.currentBucket === "") {
      throw new Error("current bucket not set");
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

  return { canUpload, upload, uploadDefault, uploading, promptUpload };
};

export default useS3;