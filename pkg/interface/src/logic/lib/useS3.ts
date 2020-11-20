import { useCallback, useMemo, useEffect, useRef } from "react";
import { S3State } from "../../types/s3-update";
import S3 from "aws-sdk/clients/s3";

export function useS3(s3: S3State) {
  const { configuration, credentials } = s3;

  const client = useRef<S3 | null>(null);

  useEffect(() => {
    if (!credentials) {
      return;
    }
    client.current = new S3({ credentials, endpoint: credentials.endpoint });
  }, [credentials]);

  const canUpload = useMemo(
    () =>
      (client && credentials && configuration.currentBucket !== "") || false,
    [credentials, configuration.currentBucket, client]
  );

  const uploadDefault = useCallback(async (file: File) => {
    if (configuration.currentBucket === "") {
      throw new Error("current bucket not set");
    }
    return upload(file, configuration.currentBucket);
  }, []);

  const upload = useCallback(
    async (file: File, bucket: string) => {
      if (!client.current) {
        throw new Error("S3 not ready");
      }

      const params = {
        Bucket: bucket,
        Key: file.name,
        Body: file,
        ACL: "public-read",
        ContentType: file.type,
      };

      const { Location } = await client.current.upload(params).promise();
      return Location;
    },
    [client]
  );

  return { canUpload, upload, uploadDefault };
}
