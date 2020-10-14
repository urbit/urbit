import React from 'react';
import { Box, InputLabel, Radio, Input } from '@tlon/indigo-react';

import GlobalApi from '~/logic/api/global';
import { S3State } from '~/types';
import { ImageInput } from '~/views/components/ImageInput';

export type BgType = "none" | "url" | "color";

export function BackgroundPicker({
  bgType,
  bgUrl,
  api,
  s3,
}: {
  bgType: BgType;
  bgUrl?: string;
  api: GlobalApi;
  s3: S3State;
}) {
  return (
    <Box>
      <InputLabel>Landscape Background</InputLabel>
      <Box display="flex" alignItems="center">
        <Box mt={3} mr={7}>
          <Radio label="Image" id="url" name="bgType" />
          {bgType === "url" && (
            <ImageInput
              api={api}
              s3={s3}
              id="bgUrl"
              name="bgUrl"
              label="URL"
              url={bgUrl || ""}
            />
          )}
          <Radio label="Color" id="color" name="bgType" />
          {bgType === "color" && (
            <Input
              ml={4}
              type="text"
              label="Color"
              id="bgColor"
              name="bgColor"
            />
          )}
          <Radio label="None" id="none" name="bgType" />
        </Box>
      </Box>
    </Box>
  );
}


