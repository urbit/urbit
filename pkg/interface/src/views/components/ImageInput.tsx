import React, { useRef, useCallback, useState } from "react";

import { Box, Input, Img, Button } from "@tlon/indigo-react";
import GlobalApi from "~/api/global";
import { useField } from "formik";
import { S3State } from "~/types/s3-update";
import { useS3 } from "~/logic/lib/useS3";

type ImageInputProps = Parameters<typeof Box>[0] & {
  id: string;
  label: string;
  s3: S3State;
};

export function ImageInput(props: ImageInputProps) {
  const { id, label, s3, ...rest } = props;

  const { uploadDefault, canUpload } = useS3(s3);

  const [uploading, setUploading] = useState(false);

  const [, , { setValue, setError }] = useField(id);

  const ref = useRef<HTMLInputElement | null>(null);

  const onImageUpload = useCallback(async () => {
    const file = ref.current?.files?.item(0);

    if (!file || !canUpload) {
      return;
    }
    try {
      setUploading(true);
      const url = await uploadDefault(file);
      setUploading(false);
      setValue(url);
    } catch (e) {
      setError(e.message);
    }
  }, [ref.current, uploadDefault, canUpload, setUploading, setValue]);

  const onClick = useCallback(() => {
    ref.current?.click();
  }, [ref]);

  return (
    <Box {...rest} display="flex">
      <Input disabled={uploading} type="text" label={label} id={id} />
      {canUpload && (
        <>
          <Button
            ml={1}
            border={3}
            borderColor="washedGray"
            style={{ marginTop: "18px" }}
            onClick={onClick}
          >
            {uploading ? "Uploading" : "Upload"}
          </Button>
          <input
            style={{ display: "none" }}
            type="file"
            id="fileElement"
            ref={ref}
            accept="image/*"
            onChange={onImageUpload}
          />
        </>
      )}
    </Box>
  );
}
