import React from 'react';
import { Box, Image } from '@tlon/indigo-react';

import { uxToHex } from '~/logic/lib/util';
import { Metadata } from '@urbit/api';
import { PropFunc } from '~/types/util';

type MetadataIconProps = PropFunc<typeof Box> & {
  metadata: Metadata;
};

export function MetadataIcon(props: MetadataIconProps) {
  const { metadata, ...rest } = props;

  const bgColor = metadata.picture ? {} : { bg: `#${uxToHex(metadata.color)}` };

  return (
    <Box {...bgColor} {...rest} borderRadius={2} boxShadow="inset 0 0 0 1px" color="lightGray" overflow="hidden">
      {metadata.picture && <Image height="100%" src={metadata.picture} />}
    </Box>
  );
}
